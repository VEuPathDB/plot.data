newScatterPD <- function(.dt = data.table::data.table(),
                         variables = new('VariableMetadataList'),
                         value = character(),
                         useGradientColorscale = FALSE,
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     useGradientColorscale = useGradientColorscale,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "scatterplot")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  xType <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  yType <- veupathUtils::findDataTypesFromPlotRef(variables, 'yAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  if (useGradientColorscale) {
    .pd$overlayMissingData <- is.na(.pd[[group]])
    series <- collapseByGroup(.pd, group = 'overlayMissingData', panel)
    .pd$overlayMissingData <- NULL
    series$overlayMissingData <- NULL
    data.table::setnames(series, c(panel, 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  } else {
    series <- collapseByGroup(.pd, group, panel)
    data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
  }
 
  if (xType == 'DATE') {
    series$seriesX <- lapply(series$seriesX, format, '%Y-%m-%d')
  } else {
    series$seriesX <- lapply(series$seriesX, as.character)
  }
  if (class(series$seriesX) != 'list') series$seriesX <- list(list(series$seriesX))

  if (yType == 'DATE') {
    series$seriesY <- lapply(series$seriesY, format, '%Y-%m-%d')
  } else {
    series$seriesY <- lapply(series$seriesY, as.character)
  }
  if (class(series$seriesY) != 'list') series$seriesY <- list(list(series$seriesY))


  if (useGradientColorscale) {
    if (identical(veupathUtils::findDataTypesFromPlotRef(variables, 'overlay'),'DATE')) {
      series$seriesGradientColorscale <- lapply(series$seriesGradientColorscale, format, '%Y-%m-%d')
    } else {
      series$seriesGradientColorscale <- lapply(series$seriesGradientColorscale, as.character)
    }
    if (class(series$seriesGradientColorscale) != 'list') series$seriesGradientColorscale <- list(list(series$seriesGradientColorscale))
  }
  
  veupathUtils::logWithTime('Collected raw scatter plot data.', verbose)

  if (value == 'smoothedMean') {
    
    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    .pd <- smoothedMean
    veupathUtils::logWithTime('Calculated smoothed means.', verbose)

  } else if (value == 'smoothedMeanWithRaw') {
    
    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, smoothedMean)
    } else {
      .pd <- cbind(series, smoothedMean)
    }
    veupathUtils::logWithTime('Calculated smoothed means.', verbose)

  } else if (value == 'bestFitLineWithRaw') {

    bestFitLine <- groupBestFitLine(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, bestFitLine)
    } else {
      .pd <- cbind(series, bestFitLine)
    }
    veupathUtils::logWithTime('Calculated best fit line.', verbose)

  } else if (value == 'density') {
    
    density <- groupDensity(.pd, NULL, x, group, panel)
    .pd <- density
    veupathUtils::logWithTime('Kernel density estimate calculated from raw data.', verbose)

  } else {
    .pd <- series
  }
  attr$names <- names(.pd)
  if (useGradientColorscale) attr$useGradientColorscale <- useGradientColorscale

  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateScatterPD <- function(.scatter, verbose) {
  variables <- attr(.scatter, 'variables')
  xShape <- veupathUtils::findDataShapesFromPlotRef(variables, 'xAxis')
  if (!xShape %in% c('CONTINUOUS')) {
    stop('The independent axis must be continuous for scatterplot.')
  }
  yShape <- veupathUtils::findDataShapesFromPlotRef(variables, 'yAxis')
  if (!yShape %in% c('CONTINUOUS')) {
    stop('The dependent axis must be continuous for scatterplot.')
  }
  veupathUtils::logWithTime('Scatter plot request has been validated!', verbose)

  return(.scatter)
}

#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'smoothedMeanX', 
#' 'smoothedMeanY' and 'smoothedMeanSE' specify the x, y and 
#' standard error respectively of the smoothed conditional mean 
#' for the group. Columns 'densityX' and 'densityY' contain the 
#' calculated kernel density estimates. Column 
#' 'seriesGradientColorscale' contains values to be used with a 
#' gradient colorscale when plotting.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data 
#' for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtil::VariableMetadataList
#' sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 
#' 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw'
#'  or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' 
#' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous 
#' overlay variable.
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = rnorm(100),
#'                  'entity.yvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- new("VariableMetadataList",
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'xvar', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'xAxis'),
#'     dataType = new("DataType", value = 'NUMBER'),
#'     dataShape = new("DataShape", value = 'CONTINUOUS')
#'   ),
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'overlay', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'overlay'),
#'     dataType = new("DataType", value = 'STRING'),
#'     dataShape = new("DataShape", value = 'CATEGORICAL')
#'   ),
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'yvar', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'yAxis'),
#'     dataType = new("DataType", value = 'NUMBER'),
#'     dataShape = new("DataShape", value = 'CONTINUOUS')
#'   )
#' )
#'  
#' # Returns a data table with plot-ready data
#' dt <- scattergl.dt(df, map, value = 'bestFitLineWithRaw')
#' @export
scattergl.dt <- function(data, 
                         variables, 
                         value = c('smoothedMean', 
                                   'smoothedMeanWithRaw', 
                                   'bestFitLineWithRaw', 
                                   'density', 
                                   'raw'),
                         evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                         collectionVariablePlotRef = NULL,
                         computedVariableMetadata = NULL,
                         verbose = c(TRUE, FALSE)) {
  
  if (!inherits(variables, 'VariableMetadataList')) stop("The `variables` argument must be a VariableMetadataList object.")
  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode) 
  verbose <- veupathUtils::matchArg(verbose)  

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type scatter.")
  } else {
    if (!xVM@dataType@value %in% c('NUMBER','INTEGER') & value == 'density') {
      stop('Density curves can only be provided for numeric independent axes.')
    }
  }

  yVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'yAxis')
  collectionVM <- veupathUtils::findCollectionVariableMetadata(variables)
  if (is.null(yVM)) {
    if (is.null(collectionVM)) {
      stop("Must provide y axis variable for plot type scatter when no collection variable is provided.")
    }
  } else {
    if (!yVM@dataType@value %in% c('NUMBER', 'INTEGER') & value != 'raw') {
      stop('Trend lines can only be provided for numeric dependent axes.')
    }
  } 

  groupVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'overlay')
  # Decide if we should use a gradient colorscale
  # For now the decision is handled internally. Eventually we may allow for this logic to be overridden and it can be a function arg.
  useGradientColorscale <- FALSE
  if (!is.null(groupVM) && !groupVM@isCollection) {
    groupColName <- veupathUtils::getColName(groupVM@variableSpec)
    if (identical(groupVM@dataShape@value, 'CONTINUOUS') && data.table::uniqueN(data[[groupColName]]) > 8) useGradientColorscale <- TRUE
  }

  if (useGradientColorscale && value != 'raw') {
    stop('Gradient colorscales cannot be used with trend lines.')
  }

  # Handle collectionVars
  if (!is.null(collectionVM)) {
    if (!collectionVM@plotReference@value %in% c('overlay', 'facet1', 'facet2')) stop('Collection variable PlotReference must be either overlayVariable, facetVariable1, or facetVariable2 for scatter.')
  }

  .scatter <- newScatterPD(.dt = data,
                            variables = variables,
                            value = value,
                            useGradientColorscale = useGradientColorscale,
                            evilMode = evilMode,
                            verbose = verbose)

  .scatter <- validateScatterPD(.scatter, verbose)
  veupathUtils::logWithTime(paste('New scatter plot object created with parameters value =', value, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.scatter)
}

#' Scatter Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'smoothedMeanX', 
#' 'smoothedMeanY' and 'smoothedMeanSE' specify the x, y and 
#' standard error respectively of the smoothed conditional mean 
#' for the group. Columns 'densityX' and 'densityY' contain the 
#' calculated kernel density estimates. Column 
#' 'seriesGradientColorscale' contains values to be used with a 
#' gradient colorscale when plotting.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for 
#' the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtil::VariableMetadataList 
#' and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 
#' 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 
#' 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw 
#' data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = rnorm(100),
#'                  'entity.yvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- new("VariableMetadataList",
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'xvar', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'xAxis'),
#'     dataType = new("DataType", value = 'NUMBER'),
#'     dataShape = new("DataShape", value = 'CONTINUOUS')
#'   ),
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'overlay', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'overlay'),
#'     dataType = new("DataType", value = 'STRING'),
#'     dataShape = new("DataShape", value = 'CATEGORICAL')
#'   ),
#'   new("VariableMetadata",
#'     variableClass = new("VariableClass", value = 'native'),
#'     variableSpec = new("VariableSpec", variableId = 'yvar', entityId = 'entity'),
#'     plotReference = new("PlotReference", value = 'yAxis'),
#'     dataType = new("DataType", value = 'NUMBER'),
#'     dataShape = new("DataShape", value = 'CONTINUOUS')
#'   )
#' )
#'  
#' # Returns the name of a json file
#' scattergl(df, map, value = 'bestFitLineWithRaw')
#' @export
scattergl <- function(data,
                      variables,
                      value = c('smoothedMean', 
                                'smoothedMeanWithRaw', 
                                'bestFitLineWithRaw', 
                                'density', 
                                'raw'),
                      evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                      verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .scatter <- scattergl.dt(data,
                           variables,
                           value = value,
                           evilMode = evilMode,
                           verbose = verbose)
                           
  outFileName <- writeJSON(.scatter, evilMode, 'scattergl', verbose)

  return(outFileName)
}

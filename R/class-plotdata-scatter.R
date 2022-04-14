newScatterPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         value = character(),
                         useGradientColorscale = FALSE,
                         evilMode = character(),
                         collectionVariableDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'collectionVariablePlotRef' = NULL),
                         computedVariableMetadata = list('displayName' = NULL,
                                                         'displayRangeMin' = NULL,
                                                         'displayRangeMax' = NULL,
                                                         'collectionVariable' = NULL),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     useGradientColorscale = useGradientColorscale,
                     evilMode = evilMode,
                     collectionVariableDetails = collectionVariableDetails,
                     computedVariableMetadata = computedVariableMetadata,
                     verbose = verbose,
                     class = "scatterplot")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  y <- veupathUtils::toColNameOrNull(attr$yAxisVariable)
  group <- veupathUtils::toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  if (identical(attr$overlayVariable$dataShape,'CONTINUOUS') && useGradientColorscale) {
    .pd$overlayMissingData <- is.na(.pd[[group]])
    series <- collapseByGroup(.pd, group = 'overlayMissingData', panel)
    .pd$overlayMissingData <- NULL
    series$overlayMissingData <- NULL
    data.table::setnames(series, c(panel, 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  } else {
    series <- collapseByGroup(.pd, group, panel)
    data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
  }
 
  if (attr$xAxisVariable$dataType == 'DATE') {
    series$seriesX <- lapply(series$seriesX, format, '%Y-%m-%d')
  } else {
    series$seriesX <- lapply(series$seriesX, as.character)
  }
  if (class(series$seriesX) != 'list') series$seriesX <- list(list(series$seriesX))

  if (attr$yAxisVariable$dataType == 'DATE') {
    series$seriesY <- lapply(series$seriesY, format, '%Y-%m-%d')
  } else {
    series$seriesY <- lapply(series$seriesY, as.character)
  }
  if (class(series$seriesY) != 'list') series$seriesY <- list(list(series$seriesY))


  if (identical(attr$overlayVariable$dataShape,'CONTINUOUS') && useGradientColorscale) {
    if (identical(attr$overlayVariable$dataType,'DATE')) {
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
  xAxisVariable <- attr(.scatter, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('CONTINUOUS')) {
    stop('The independent axis must be continuous for scatterplot.')
  }
  yAxisVariable <- attr(.scatter, 'yAxisVariable')
  if (!yAxisVariable$dataShape %in% c('CONTINUOUS')) {
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
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' - naToZero: Options are TRUE, FALSE, or ''. Optional. Indicates whether to replaces NAs with 0, assuming the column is numeric. If set to TRUE, all NAs found within the column should be replaced with 0. Passing '' will result in using the function default, which in plot.data is FALSE. Setting naToZero=TRUE for a string var will throw an error. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable 
#' sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 
#' 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw'
#'  or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' 
#' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous 
#' overlay variable.
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of 
#' variables should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = rnorm(100),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CONTINUOUT', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- scattergl.dt(df, map, value = 'bestFitLineWithRaw')
#' @export
scattergl.dt <- function(data, 
                         map, 
                         value = c('smoothedMean', 
                                   'smoothedMeanWithRaw', 
                                   'bestFitLineWithRaw', 
                                   'density', 
                                   'raw'),
                         evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                         collectionVariablePlotRef = NULL,
                         computedVariableMetadata = NULL,
                         verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode) 
  verbose <- veupathUtils::matchArg(verbose)  

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  map <- validateMap(map)
  veupathUtils::logWithTime('Map has been validated.', verbose)

  # If there is a duplicated plotRef in map, it must match collectionVariablePlotRef
  if (any(duplicated(map$plotRef))) {
    if (!identical(collectionVariablePlotRef, unique(map$plotRef[duplicated(map$plotRef)]))) {
      stop('collectionVar error: duplicated map plotRef does not match collectionVariablePlotRef.')
    }
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type scatter.")
  } else {
    if (!xAxisVariable$dataType %in% c('NUMBER','INTEGER') & value == 'density') {
      stop('Density curves can only be provided for numeric independent axes.')
    }
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    if (is.null(collectionVariablePlotRef)) {
      stop("Must provide xAxisVariable for plot type scatter.")
    }
  } else {
    if (!yAxisVariable$dataType %in% c('NUMBER', 'INTEGER') & value != 'raw') {
      stop('Trend lines can only be provided for numeric dependent axes.')
    }
  } 
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')

  # Decide if we should use a gradient colorscale
  # For now the decision is handled internally. Eventually we may allow for this logic to be overridden and it can be a function arg.
  useGradientColorscale <- FALSE
  if (!is.null(overlayVariable$variableId)) {
    overlayVariableColumnName <- veupathUtils::toColNameOrNull(overlayVariable)
    if (identical(overlayVariable$dataShape, 'CONTINUOUS') && data.table::uniqueN(data[[overlayVariableColumnName]]) > 8) useGradientColorscale <- TRUE
  }

  if (useGradientColorscale && value != 'raw') {
    stop('Gradient colorscales cannot be used with trend lines.')
  }

  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  # Handle collectionVars
  collectionVariableDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'collectionVariablePlotRef' = collectionVariablePlotRef)
  if (!is.null(collectionVariablePlotRef)) {
    if (identical(collectionVariablePlotRef, 'overlayVariable')) { 
      inferredVarEntityId <- unique(overlayVariable$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable1')) { 
      inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable2')) { 
      inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { 
      stop('collectionVar error: collectionVariablePlotRef must be either overlayVariable, facetVariable1, or facetVariable2 for scatter.')
    }

    collectionVariableDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS')

    veupathUtils::logWithTime('Created inferred variable from collectionVariable.', verbose)
  }

  .scatter <- newScatterPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value = value,
                            useGradientColorscale = useGradientColorscale,
                            evilMode = evilMode,
                            collectionVariableDetails = collectionVariableDetails,
                            computedVariableMetadata = computedVariableMetadata,
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
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' - naToZero: Options are TRUE, FALSE, or ''. Optional. Indicates whether to replaces NAs with 0, assuming the column is numeric. If set to TRUE, all NAs found within the column should be replaced with 0. Passing '' will result in using the function default, which in plot.data is FALSE. Setting naToZero=TRUE for a string var will throw an error. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId 
#' and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 
#' 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 
#' 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw 
#' data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of variables 
#' should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = rnorm(100),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CONTINUOUT', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns the name of a json file
#' scattergl(df, map, value = 'bestFitLineWithRaw')
#' @export
scattergl <- function(data,
                      map,
                      value = c('smoothedMean', 
                                'smoothedMeanWithRaw', 
                                'bestFitLineWithRaw', 
                                'density', 
                                'raw'),
                      evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                      collectionVariablePlotRef = NULL,
                      computedVariableMetadata = NULL,
                      verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .scatter <- scattergl.dt(data,
                           map,
                           value = value,
                           evilMode = evilMode,
                           collectionVariablePlotRef = collectionVariablePlotRef,
                           computedVariableMetadata = computedVariableMetadata,
                           verbose = verbose)
                           
  outFileName <- writeJSON(.scatter, evilMode, 'scattergl', verbose)

  return(outFileName)
}

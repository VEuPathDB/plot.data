newBoxPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),
                         points = character(),
                         mean = logical(),
                         computeStats = logical(),
                         overlayValues = character(),
                         sampleSizes = logical(),
                         completeCases = logical(),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     overlayValues = overlayValues,
                     sampleSizes = sampleSizes,
                     completeCases = completeCases,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "boxplot")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  .pd[[x]] <- as.character(.pd[[x]])

  summary <- groupSummary(.pd, x, y, group, panel)
  fences <- groupFences(.pd, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]
  veupathUtils::logWithTime('Calculated five-number summaries and upper and lower fences for boxplot.', verbose)

  isEvil <- ifelse(evilMode %in% c('allVariables', 'strataVariables'), TRUE, FALSE)

  if (!isEvil && computeStats) {
    
    if (is.null(group)) {
      # If no overlay, then compute across x per panel
      statsTable <- nonparametricByGroup(.pd, numericCol=y, levelsCol=x, byCols=panel)
      
    } else {
      # compute across overlay values per panel
      statsTable <- nonparametricByGroup(.pd, numericCol=y, levelsCol=group, byCols=c(x, panel))
    }
    
    attr$statsTable <- statsTable
    veupathUtils::logWithTime('Calculated boxplot supporting statistics.', verbose)
  }
  

  if (!is.null(key(summary))) {
    .pd.base <- merge(summary, fences)
  } else {
    .pd.base <- cbind(summary, fences)
  }

  if (points == 'outliers') {
    outliers <- groupOutliers(.pd, x, y, group, panel)
    outliers[[x]] <- NULL
    if (!is.null(key(outliers))) {
      .pd.base <- merge(.pd.base, outliers)
    } else {
      .pd.base <- cbind(.pd.base, outliers)
    }
    veupathUtils::logWithTime('Identified outliers for boxplot.', verbose)
  } else if (points == 'all') {
    byCols <- colnames(.pd)[colnames(.pd) %in% c(x, group, panel)]
    rawData <- .pd[, list(rawData=lapply(.SD, as.vector)), keyby=byCols]
    byColValues <- unique(.pd[, byCols, with=FALSE])
    rawData <- merge(rawData, byColValues, by=byCols, all=TRUE)

    rawData <- collapseByGroup(rawData, group, panel)
    rawData[[x]] <- NULL
    indexCols <- c(panel, group)
    setkeyv(rawData, indexCols)

    if (!is.null(key(rawData))) {
      .pd.base <- merge(.pd.base, rawData)
    } else {
      .pd.base <- cbind(.pd.base, rawData)
    }
    veupathUtils::logWithTime('Returning all points for boxplot.', verbose)
  }

  if (mean) {
    mean <- groupMean(.pd, x, y, group, panel)
    mean[[x]] <- NULL
    if (!is.null(key(mean))) {
      .pd.base <- merge(.pd.base, mean)
    } else {
      .pd.base <- cbind(.pd.base, mean)
    }
    veupathUtils::logWithTime('Calculated means for boxplot.', verbose)
  }
  
  .pd <- .pd.base
  data.table::setnames(.pd, x, 'label')

  attr$names <- names(.pd)
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateBoxPD <- function(.box, verbose) {
  variables <- attr(.box, 'variables')
  if (!veupathUtils::findDataTypesFromPlotRef(variables, 'yAxis') %in% c('NUMBER', 'INTEGER')) {
    stop('The dependent axis must be of type number or integer for boxplot.')
  }
  veupathUtils::logWithTime('Boxplot request has been validated!', verbose)

  return(.box)
}


#' Box Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils VariableMetadataList
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param overlayValues character vector providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- veupathUtils::VariableMetadataList(
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'xvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'xAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'overlay', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'overlay'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'NUMBER'),
#'     dataShape = veupathUtils::DataShape(value = 'CONTINUOUS')
#'   )
#' )
#' 
#' # Returns a data table with plot-ready data
#' dt <- box.dt(df, map, points = 'outliers', mean=F, computeStats=T)
#' @export

box.dt <- function(data, variables, 
                   points = c('outliers', 'all', 'none'), 
                   mean = c(FALSE, TRUE), 
                   computeStats = c(FALSE, TRUE), 
                   overlayValues = NULL,
                   sampleSizes = c(TRUE, FALSE),
                   completeCases = c(TRUE, FALSE),
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {

  points <- veupathUtils::matchArg(points)
  mean <- veupathUtils::matchArg(mean)
  computeStats <- veupathUtils::matchArg(computeStats)
  sampleSizes <- veupathUtils::matchArg(sampleSizes)
  completeCases <- veupathUtils::matchArg(completeCases)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  isEvil <- ifelse(evilMode %in% c('allVariables', 'strataVariables'), TRUE, FALSE)
  if (isEvil && computeStats) {
    warning('evilModes `allVariables` and `strataVariables` are not compatible with computeStats! computeStats will be ignored!')
  }

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type box.")
  }
  
  yVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'yAxis')
  collectionVM <- veupathUtils::findCollectionVariableMetadata(variables)
  if (is.null(yVM) & is.null(collectionVM)) {
    stop("Must provide y-axis variable for plot type box.")
  }
  
  # Handle collectionVars
  if (!is.null(collectionVM)) {
    if (!collectionVM@plotReference@value %in% c('xAxis', 'facet1', 'facet2')) stop('Collection variable PlotReference must be either xAxis, facet1, or facet2 for boxplot.')
  }


  .box <- newBoxPD(.dt = data,
                    variables = variables,
                    points = points,
                    mean = mean,
                    computeStats = computeStats,
                    overlayValues = overlayValues,
                    sampleSizes = sampleSizes,
                    completeCases = completeCases,
                    evilMode = evilMode,
                    verbose = verbose)

  .box <- validateBoxPD(.box, verbose)
  veupathUtils::logWithTime(paste('New boxplot object created with parameters points =', points,
                                                                              ', mean =', mean,
                                                                              ', computeStats =', computeStats,
                                                                              ', sampleSizes = ', sampleSizes,
                                                                              ', completeCases = ', completeCases,
                                                                              ', evilMode =', evilMode,
                                                                              ', verbose =', verbose), verbose)

  return(.box)

}

#' Box Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils VariableMetadataList
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param overlayValues character vector providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- veupathUtils::VariableMetadataList(
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'xvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'xAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'overlay', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'overlay'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'NUMBER'),
#'     dataShape = veupathUtils::DataShape(value = 'CONTINUOUS')
#'   )
#' )
#' 
#' # Returns the name of a json file
#' box(df, map, points = 'outliers', mean=F, computeStats=T)
#' @export
box <- function(data, variables, 
                points = c('outliers', 'all', 'none'), 
                mean = c(FALSE, TRUE), 
                computeStats = c(FALSE, TRUE), 
                overlayValues = NULL,
                sampleSizes = c(TRUE, FALSE),
                completeCases = c(TRUE, FALSE),
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .box <- box.dt(data,
                 variables = variables,
                 points = points,
                 mean = mean,
                 computeStats = computeStats,
                 overlayValues = overlayValues,
                 sampleSizes = sampleSizes,
                 completeCases = completeCases,
                 evilMode = evilMode,
                 verbose = verbose)
  outFileName <- writeJSON(.box, evilMode, 'boxplot', verbose)

  return(outFileName)
}

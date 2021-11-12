newBoxPD <- function(.dt = data.table::data.table(),
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
                         points = character(),
                         mean = logical(),
                         computeStats = logical(),
                         evilMode = logical(),
                         listVarDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'listVarPlotRef' = NULL,
                                               'listVarDisplayLabel' = NULL),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     listVarDetails = listVarDetails,
                     verbose = verbose,
                     class = "boxplot")

  attr <- attributes(.pd)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  #remove after sorting out #88, fixing updateTypes
  .pd[[y]] <- as.numeric(.pd[[y]])
  summary <- groupSummary(.pd, x, y, group, panel)
  fences <- groupFences(.pd, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]
  veupathUtils::logWithTime('Calculated five-number summaries and upper and lower fences for boxplot.', verbose)

  if (!evilMode && computeStats) {
    
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
  xAxisVariable <- attr(.box, 'xAxisVariable')
  #if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #  stop('The independent axis must be binary, ordinal or categorical for boxplot.')
  #}
  yAxisVariable <- attr(.box, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER', 'INTEGER')) {
    stop('The dependent axis must be of type number or integer for boxplot.')
  }
  overlayVariable <- attr(.box, 'overlayVariable')
  #if (!is.null(overlayVariable)) {
  #  if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The overlay variable must be binary, ordinal or categorical.')
  #  }
  #}
  facetVariable1 <- attr(.box, 'facetVariable1')
  #if (!is.null(facetVariable1)) {
  #  if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The first facet variable must be binary, ordinal or categorical.')
  #  }
  #}
  facetVariable2 <- attr(.box, 'facetVariable2')
  #if (!is.null(facetVariable2)) {
  #  if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The second facet variable must be binary, ordinal or categorical.')
  #  }
  #}
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
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- box.dt(df, map, points = 'outliers', mean=F, computeStats=T)
#' @export

box.dt <- function(data, map, 
                   points = c('outliers', 'all', 'none'), 
                   mean = c(FALSE, TRUE), 
                   computeStats = c(FALSE, TRUE), 
                   evilMode = c(FALSE, TRUE),
                   listVarPlotRef = NULL,
                   listVarDisplayLabel = NULL,
                   inferredVarDisplayLabel = NULL,
                   verbose = c(TRUE, FALSE)) {

  points <- veupathUtils::matchArg(points)
  mean <- veupathUtils::matchArg(mean)
  computeStats <- veupathUtils::matchArg(computeStats)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (evilMode && computeStats) {
    warning('evilMode and computeStats are not compatible! computeStats will be ignored!')
  }

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  map <- validateMap(map)
  veupathUtils::logWithTime('Map has been validated.', verbose)

  # If there is a duplicated plotRef in map, it must match listVarPlotRef
  if (any(duplicated(map$plotRef))) {
    if (!identical(listVarPlotRef, unique(map$plotRef[duplicated(map$plotRef)]))) {
      stop('listVar error: duplicated map plotRef does not match listVarPlotRef.')
    }
  }

  # If listVar and inferredVar labels are provided, must also provide listVarPlotRef
  if ((!is.null(listVarDisplayLabel) | !is.null(inferredVarDisplayLabel)) & is.null(listVarPlotRef)) {
    stop('listVar error: listVarPlotRef must be specified in order to use inferredVarDisplayLabel or listVarDisplayLabel')
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type box.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId) & is.null(listVarPlotRef)) {
    stop("Must provide yAxisVariable for plot type box.")
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  
  # Handle listVars
  listVarDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'listVarPlotRef' = listVarPlotRef,
                         'listVarDisplayLabel' = listVarDisplayLabel)

  if (!is.null(listVarPlotRef)) {
    if (identical(listVarPlotRef, 'xAxisVariable')) { inferredVarEntityId <- unique(xAxisVariable$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable1')) { inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable2')) { inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { stop('listVar error: listVarPlotRef must be either xAxisVariable, facetVariable1, or facetVariable2 for box.')
    }

    listVarDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS',
                                          'displayLabel' = inferredVarDisplayLabel)

    veupathUtils::logWithTime('Created inferred variable from listVariable.', verbose)
  }

  .box <- newBoxPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points = points,
                    mean = mean,
                    computeStats = computeStats,
                    evilMode = evilMode,
                    listVarDetails = listVarDetails,
                    verbose = verbose)

  .box <- validateBoxPD(.box, verbose)
  veupathUtils::logWithTime(paste('New boxplot object created with parameters points =', points, ', mean =', mean, ', computeStats =', computeStats, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

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
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns the name of a json file
#' box(df, map, points = 'outliers', mean=F, computeStats=T)
#' @export
box <- function(data, map, 
                points = c('outliers', 'all', 'none'), 
                mean = c(FALSE, TRUE), 
                computeStats = c(FALSE, TRUE), 
                evilMode = c(FALSE, TRUE),
                listVarPlotRef = NULL,
                listVarDisplayLabel = NULL,
                inferredVarDisplayLabel = NULL,
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .box <- box.dt(data,
                 map,
                 points = points,
                 mean = mean,
                 computeStats = computeStats,
                 evilMode = evilMode,
                 listVarPlotRef = listVarPlotRef,
                 listVarDisplayLabel = listVarDisplayLabel,
                 inferredVarDisplayLabel = inferredVarDisplayLabel,
                 verbose = verbose)
  outFileName <- writeJSON(.box, evilMode, 'boxplot', verbose)

  return(outFileName)
}

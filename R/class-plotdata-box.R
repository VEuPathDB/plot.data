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
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     class = "boxplot")

  attr <- attributes(.pd)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  summary <- groupSummary(.pd, x, y, group, panel)
  fences <- groupFences(.pd, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]

  if (computeStats) {
    
    if (is.null(group)) {
      # If no overlay, then compute across x per panel
      statsTable <- nonparametricByGroup(.pd, numericCol=y, levelsCol=x, byCols=panel)
      
    } else {
      # compute across overlay values per panel
      statsTable <- nonparametricByGroup(.pd, numericCol=y, levelsCol=group, byCols=c(x, panel))
    }
    
    attr$statsTable <- statsTable
    
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
  }

  if (mean) {
    mean <- groupMean(.pd, x, y, group, panel)
    mean[[x]] <- NULL
    if (!is.null(key(mean))) {
      .pd.base <- merge(.pd.base, mean)
    } else {
      .pd.base <- cbind(.pd.base, mean)
    }
  }
  
  .pd <- .pd.base
  data.table::setnames(.pd, x, 'label')
  attr$names <- names(.pd)
  setAttrFromList(.pd, attr)

  return(.pd)
}

validateBoxPD <- function(.box) {
  xAxisVariable <- attr(.box, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
    stop('The independent axis must be binary, ordinal or categorical for boxplot.')
  }
  yAxisVariable <- attr(.box, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number for boxplot.')
  }
  overlayVariable <- attr(.box, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The overlay variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable1 <- attr(.box, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.box, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }

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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @return data.table plot-ready data
#' @export
box.dt <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE), computeStats = c(TRUE, FALSE), evilMode = c(FALSE, TRUE)) {

  points <- matchArg(points)
  mean <- matchArg(mean)
  computeStats <- matchArg(computeStats)
  evilMode <- matchArg(evilMode)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  # Handle repeated plot references
  if (any(duplicated(map$plotRef))) {
    
    # Identify the list var based on any plotRef that is repeated
    listVarPlotRef <- unique(map$plotRef[duplicated(map$plotRef)])
    listVarPlotRef <- validateListVar(map, listVarPlotRef)
    
    # Box-specific
    if (listVarPlotRef == 'xAxisVariable' | listVarPlotRef == 'facetVariable1') {
      meltedValuePlotRef <- 'yAxisVariable'
    } else {
      stop("Incompatable repeated variable")
    }
    
    # Check to ensure meltedValuePlotRef is not already defined
    if (any(map$plotRef == meltedValuePlotRef)) {
      stop(paste0("Cannot melt data: ", meltedValuePlotRef, " already defined."))
    }

    # Record variable order
    listVarIdOrder <- map$id[map$plotRef == listVarPlotRef]
    
    # Melt data and update the map 
    data <- data.table::melt(data, measure.vars = listVarIdOrder, variable.factor = FALSE, variable.name='meltedVariable', value.name='meltedValue')
    map <- remapListVar(map, listVarPlotRef, meltedValuePlotRef)
    
  } # end handling of repeated plot element references

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type box.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    stop("Must provide yAxisVariable for plot type box.")
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .box <- newBoxPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points = points,
                    mean = mean,
                    computeStats = computeStats,
                    evilMode = evilMode)

  .box <- validateBoxPD(.box)

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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param computeStats boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @return character name of json file containing plot-ready data
#' @export
box <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE), computeStats = c(TRUE, FALSE), evilMode = c(FALSE, TRUE)) {

  points <- matchArg(points)
  mean <- matchArg(mean)
  computeStats <- matchArg(computeStats)
  evilMode <- matchArg(evilMode)

  .box <- box.dt(data, map, points, mean, computeStats, evilMode)
  outFileName <- writeJSON(.box, evilMode, 'boxplot')

  return(outFileName)
}

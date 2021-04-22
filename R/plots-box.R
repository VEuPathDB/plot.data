newBoxPD <- function(.dt = data.table::data.table(),
                         independentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         dependentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         points = character(),
                         mean = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     independentVar = independentVar,
                     dependentVar = dependentVar,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "boxplot")

  attr <- attributes(.pd)

  independent <- attr$independentVar$variableId
  dependent <- attr$dependentVar$variableId
  overlay <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  summary <- groupSummary(.pd, independent, dependent, overlay, panel)
  fences <- groupFences(.pd, independent, dependent, overlay, panel)
  fences <- fences[, -independent, with = FALSE]
  if (!is.null(key(summary))) {
    .pd.base <- merge(summary, fences)
  } else {
    .pd.base <- cbind(summary, fences)
  }

  if (points == 'outliers') {
    points <- groupOutliers(.pd, independent, dependent, overlay, panel)
    points[[independent]] <- NULL
    if (!is.null(key(points))) {
      .pd.base <- merge(.pd.base, points)
    } else {
      .pd.base <- cbind(.pd.base, points)
    }
  } else if (points == 'all') {
    rawData <- noStatsFacet(.pd, overlay, panel)
    names(rawData)[names(rawData) == dependent] <- 'series.dependent'
    names(rawData)[names(rawData) == independent] <- 'series.independent'
    if (!is.null(key(rawData))) {
      .pd.base <- merge(.pd.base, rawData)
    } else {
      .pd.base <- cbind(.pd.base, rawData)
    }
  }

  if (mean) {
    mean <- groupMean(.pd, independent, dependent, overlay, panel)
    mean[[independent]] <- NULL
    if (!is.null(key(mean))) {
      .pd.base <- merge(.pd.base, mean)
    } else {
      .pd.base <- cbind(.pd.base, mean)
    }
  }
  .pd <- .pd.base
  attr$names <- names(.pd)

  attributes(.pd) <- attr

  return(.pd)
}

validateBoxPD <- function(.box) {
  independentVar <- attr(.box, 'independentVar')
  if (!independentVar$dataType %in% c('STRING')) {
    stop('The independent axis must be of type string for boxplot.')
  }
  dependentVar <- attr(.box, 'dependentVar')
  if (!dependentVar$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number for boxplot.')
  }

  return(.box)
}

#' Box Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per overlay (per panel). Columns 
#' 'independent', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'overlay' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per overlay (per panel)
#' @return data.table plot-ready data
#' @export
box.dt <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }

  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)

  if (!'data.table' %in% class(data)) {
    data <- data.table::as.data.table(data)
  }

  if ('independentVar' %in% map$plotRef) {
    independentVar <- plotRefMapToList(map, 'independentVar')
  } else {
    stop("Must provide independentVar for plot type box.")
  }
  if ('dependentVar' %in% map$plotRef) {
    dependentVar <- plotRefMapToList(map, 'dependentVar')
  } else {
    stop("Must provide dependentVar for plot type box.")
  }
  if ('overlayVariable' %in% map$plotRef) {
    overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }

  .box <- newBoxPD(.dt = data,
                    independentVar = independentVar,
                    dependentVar = dependentVar,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points,
                    mean)

  .box <- validateBoxPD(.box)

  return(.box) 

}

#' Box Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per overlay (per panel). Columns 
#' 'independent', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'overlay' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per overlay (per panel)
#' @return character name of json file containing plot-ready data
#' @export
box <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }
  .box <- box.dt(data, map, points, mean)
  outFileName <- writeJSON(.box, 'boxplot')

  return(outFileName)
}

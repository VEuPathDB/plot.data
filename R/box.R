newBoxPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         yAxisVariable = list('variableId' = NULL,
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
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "boxplot")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  summary <- groupSummary(.pd, x, y, group, panel)
  fences <- groupFences(.pd, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]
  if (!is.null(key(summary))) {
    .pd.base <- merge(summary, fences)
  } else {
    .pd.base <- cbind(summary, fences)
  }

  if (points == 'outliers') {
    points <- groupOutliers(.pd, x, y, group, panel)
    points[[x]] <- NULL
    if (!is.null(key(points))) {
      .pd.base <- merge(.pd.base, points)
    } else {
      .pd.base <- cbind(.pd.base, points)
    }
  } else if (points == 'all') {
    #TODO make sure series.x and x have same format, both are unique lists of xaxis entries
    rawData <- noStatsFacet(.pd, group, panel)
    names(rawData)[names(rawData) == y] <- 'series.y'
    names(rawData)[names(rawData) == x] <- 'series.x'
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
  attr$names <- names(.pd)

  attributes(.pd) <- attr

  return(.pd)
}

validateBoxPD <- function(.box) {
  xAxisVariable <- attr(.box, 'xAxisVariable')
  if (!xAxisVariable$dataType %in% c('STRING')) {
    stop('The independent axis must be of type string for boxplot.')
  }
  yAxisVariable <- attr(.box, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number for boxplot.')
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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return data.table plot-ready data
#' @export
box.dt <- function(data, map, points, mean) {
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

  #TODO make helper for the map -> list conversion
  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- list('variableId' = map$id[map$plotRef == 'xAxisVariable'],
                          'entityId' = map$entityId[map$plotRef == 'xAxisVariable'],
                          'dataType' = map$dataType[map$plotRef == 'xAxisVariable'])
  } else {
    stop("Must provide xAxisVariable for plot type box.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- list('variableId' = map$id[map$plotRef == 'yAxisVariable'],
                          'entityId' = map$entityId[map$plotRef == 'yAxisVariable'],
                          'dataType' = map$dataType[map$plotRef == 'yAxisVariable'])
  } else {
    stop("Must provide yAxisVariable for plot type box.")
  }
  if ('overlayVariable' %in% map$plotRef) {
    overlayVariable <- list('variableId' = map$id[map$plotRef == 'overlayVariable'],
                            'entityId' = map$entityId[map$plotRef == 'overlayVariable'],
                            'dataType' = map$dataType[map$plotRef == 'overlayVariable'])
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- list('variableId' = map$id[map$plotRef == 'facetVariable1'],
                           'entityId' = map$entityId[map$plotRef == 'facetVariable1'],
                           'dataType' = map$dataType[map$plotRef == 'facetVariable1'])
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- list('variableId' = map$id[map$plotRef == 'facetVariable2'],
                           'entityId' = map$entityId[map$plotRef == 'facetVariable2'],
                           'dataType' = map$dataType[map$plotRef == 'facetVariable2'])
  }

  .box <- newBoxPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
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
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return character name of json file containing plot-ready data
#' @export
box <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  mean <- match.arg(mean)
  .box <- box.dt(data, map, points, mean)
  outFileName <- writeJSON(.box, 'boxplot')

  return(outFileName)
}

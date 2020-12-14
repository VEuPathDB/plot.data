#' Box Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return data.table plot-ready data
#' @export
box.dt <- function(data, map, points, mean) {
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(y, x, group, panel)
  data <- data[, myCols, with=FALSE]

  summary <- groupSummary(data, x, y, group, panel)
  #summary <- summary[, -c('min', 'max'), with=FALSE]
  fences <- groupFences(data, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]
  data.base <- merge(summary, fences)

  if (points == 'outliers') {
    points <- groupOutliers(data, x, y, group, panel)
    data.base <- merge(data.base, points)
  } else if (points == 'all') {
    #TODO make sure series.x and x have some format, both are unique lists of xaxis entries
    rawData <- noStatsFacet(data.back, group, panel)
    names(rawData)[names(rawData) == y] <- 'series.y'
    names(rawData)[names(rawData) == x] <- 'series.x'
    data.base <- merge(data.base, rawData)
  }

  if (mean) {
    mean <- groupMean(data, x, y, group, panel)
    data.base <- merge(data.base, mean)
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data.base, data.back)
  } else {
    data <- cbind(data.base, data.back)
  }

  return(data)
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
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return character name of json file containing plot-ready data
#' @export
box <- function(data, map, points = c('outliers', 'all', 'none'), mean = FALSE) {
  points <- match.arg(points)
  dt <- box.dt(data, map, points, mean)
  outFileName <- writeJSON(dt, 'boxplot')

  return(outFileName)
}

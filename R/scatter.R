#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'series.x' and 'series.y' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.x', 
#' 'interval.y' and 'interval.se' specify the x, y and standard error
#'  respectively of the smoothed conditional mean for the group. 
#'  Columns 'density.x' and 'density.y' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, map, value) {
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

  incompleteCaseCount <- nrow(data[!complete.cases(data),])
  data <- data[complete.cases(data),]

  series <- noStatsFacet(data, group, panel)
  names(series) <- c(group, panel, 'series.y', 'series.x')

  # TODO determine if we always want series, or if it depends on whats selected
  if (value == 'smoothedMean') {
    interval <- groupSmoothedMean(data, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel)
    if (!is.null(key(series))) {
      data <- merge(series, interval)
    } else {
      data <- cbind(series, interval)
    }
  } else if (value == 'density') {
    density <- groupDensity(data, x, group, panel)
    names(density) <- c(group, panel, 'density.x', 'density.y')
    if (!is.null(key(series))) {
      data <- merge(series, density)
    } else {
      data <- cbind(series, density)
    }
  } else {
    data <- series
  }

  #data.back <- noStatsFacet(data.back, group, panel)
  #data.back <- data.back[, -c(y, x), with = FALSE]
  #if (!is.null(key(data.back))) {
  #  data <- merge(data, data.back)
  #} else {
  #  data <- cbind(data, data.back)
  #}

  return(list(data, incompleteCaseCount))
}

#' Scatter Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'series.x' and 'series.y' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.x', 
#' 'interval.y' and 'interval.se' specify the x, y and standard error
#'  respectively of the smoothed conditional mean for the group.
#'  Columns 'density.x' and 'density.y' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, value = c('smoothedMean', 'density', 'raw')) {
  value <- match.arg(value)
  outList <- scattergl.dt(data, map, value)
  dt <- outList[[1]]
  namedAttrList <- list('incompleteCases' = jsonlite::unbox(outList[[2]]))

  outFileName <- writeJSON(dt, 'scattergl', namedAttrList, map)

  return(outFileName)
}

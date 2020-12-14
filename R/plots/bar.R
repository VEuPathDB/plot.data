#' Bar Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are two options to calculate y-values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurances of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value String indicating how to calculate y-values ('identity', 'count')
#' @return data.table plot-ready data
#' @export
bar.dt <- function(data, map, value) {
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(x, group, panel)
  data <- data[, myCols, with=FALSE]

  if (value == 'identity') {
    data <- noStatsFacet(data, group, panel)
  } else if (value == 'count' ) {
    data$dummy <- 1
    data <- groupSize(data, x, 'dummy', group, panel)
    names(data) <- c('label', group, panel, 'value')
  } else {
    stop('Unrecognized argument to "value".')
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -x, with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Bar Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are two options to calculate y-values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurances of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value String indicating how to calculate y-values ('identity', 'count')
#' @return character name of json file containing plot-ready data
#' @export
bar <- function(data, map, value = c('count', 'identity')) {
  value <- match.arg(value)
  dt <- bar.dt(data, map, value)
  outFileName <- writeJSON(data, 'barplot')

  return(outFileName)
}

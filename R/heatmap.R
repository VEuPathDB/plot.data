#' Heatmap as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @return data.table plot-ready data
#' @export
heatmap.dt <- function(data, map, value) {
  #NOTE: one or the other of these could be a list for 'collection'
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  #NOTE: this for the case of 'series'
  z <- emptyStringToNull(map$id[map$plotRef == 'zAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(y, x, z, panel)
  data <- data[, myCols, with=FALSE]
  
  if (value == 'collection') {
    data <- groupSplit(data, x, y, NULL, NULL, panel)
  } else if (value == 'series' ) { 
    data <- data[order(data[[x]]),]
    data[[x]] <- as.factor(data[[x]])
    data[[y]] <- as.factor(data[[y]])
    data <- groupSplit(data, x, y, z, NULL, panel, longToWide = TRUE)
  } else {
    stop('Unrecognized argument to "value".')
  } 
  
  #data.back <- noStatsFacet(data.back, NULL, panel)
  #data.back <- data.back[, -c(y, x, z), with = FALSE]
  #if (!is.null(key(data.back))) {
  #  data <- merge(data, data.back)
  #} else {
  #  data <- cbind(data, data.back)
  #} 
  
  return(data)
}

#' Heatmap data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @return character name of json file containing plot-ready data
#' @export
heatmap <- function(data, map, value = c('series','collection')) {
  value <- match.arg(value)
  dt <- heatmap.dt(data, map, value)
  outFileName <- writeJSON(dt, 'heatmap')

  return(outFileName)
}

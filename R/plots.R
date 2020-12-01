# TODO move density into scatter. switch smoothedMean to value

#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'series.x' and 'series.y' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.x', 
#' 'interval.y' and 'interval.se' specify the x, y and standard error
#'  respectively of the smoothed conditional mean for the group.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param smoothedMean boolean indicating whether to calculate smoothed mean
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, map, smoothedMean) {
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

  series <- noStatsFacet(data, group, panel)
  names(series) <- c(group, panel, 'series.y', 'series.x') 

  if (smoothedMean) {
    interval <- groupSmoothedMean(data, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel) 
    if (!is.null(key(series))) {
      data <- merge(series, interval)
    } else {
      data <- cbind(series, interval)
    }
  } else {
    data <- series
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param smoothedMean boolean indicating whether to calculate smoothed mean
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, smoothedMean = FALSE) {
  dt <- scattergl.dt(data, map, smoothedMean)
  outFileName <- writeJSON(dt, 'scattergl')

  return(outFileName)
}


#' Density Plot as data.table
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'series.x' and 'series.y' contain the calculated kernel density 
#' estimates. Column 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @return data.table plot-ready data
#' @export
density <- function(data, map) {
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(y, x, group, panel)
  data <- data[, myCols, with=FALSE]

  data <- groupDensity(data, x, group, panel)
  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}


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

  data.back <- noStatsFacet(data.back, NULL, panel)
  data.back <- data.back[, -c(y, x, z), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

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
heatmap <- function(data, map, value = 'series') {
  dt <- heatmap.dt(data, map, value)
  outFileName <- writeJSON(dt, 'heatmap')

  return(outFileName)
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
box <- function(data, map, points = 'outliers', mean = FALSE) {
  dt <- box.dt(data, map, points, mean)
  outFileName <- writeJSON(data, 'boxplot')

  return(outFileName)
}


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
bar <- function(data, map, value = 'identity') {
  dt <- bar.dt(data, map, value)
  outFileName <- writeJSON(data, 'barplot')

  return(outFileName)
}


#' Histogram as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @return data.table plot-ready data
#' @export
histogram.dt <- function(data, map, binWidth, value) {
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

  if (is.character(binWidth)) {
    data[[x]] <- as.POSIXct(data[[x]], format = "%Y-%m-%d")
  }

  if (value == 'count') {
    data <- binSize(data, x, group, panel, binWidth)
  } else if (value == 'proportion' ) {
    data <- binProportion(data, x, group, panel, binWidth)
  } else {
    stop('Unrecognized argument to "value".')
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Histogram data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @return character name of json file containing plot-ready data
#' @export
histogram <- function(data, map, binWidth = .1, value = 'count') {
  dt <- histogram.dt(data, map, binWidth, value)
  outFileName <- writeJSON(data, 'histogram')

  return(outFileName)
}


#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @return data.table plot-ready data
#' @export
mosaic.dt <- function(data, map) {
  group <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(x, group, panel)
  data <- data[, myCols, with=FALSE]
  
  dims <- as.data.frame.matrix(table(data[[x]], data[[group]]))
  dims <- c(length(dims), nrow(dims))

  if (any(dims > 2)) {
    data <- panelChiSq(data, x, group, panel)
  } else {
    data <- panelBothRatios(data, x, group, panel)  
  }

   #TODO check this works, since we only have panels here really
  data.back <- noStatsFacet(data.back, NULL, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @return character name of json file containing plot-ready data
#' @export
mosaic <- function(data, map) {
  dt <- mosaic.dt(data, map)
  outFileName <- writeJSON(data, 'mosaic')

  return(outFileName)
}

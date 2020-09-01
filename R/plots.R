# TODO make sure column names in 'data' are right ('x', 'y', 'color' etc)
# TODO make sure each plot type has all needed map entries
## ex line may need x and y just to relabel the columns ??

#' Scatter Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'series.x' and 'series.y'
#' contain the raw data for the scatter plot. Column 'group' and 
#' 'panel' specify the group the series data belongs to. 
#' Optionally, columns 'interval.x', 'interval.y' and 'interval.se'
#' specify the x, y and standard error respectively of the smoothed
#' conditional mean for the group.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param smoothedMean boolean indicating whether to calculate smoothed mean
#' @return data.table of plot-ready data
#' @export
scattergl <- function(data, map, smoothedMean = FALSE) {
  group <- map$id[map$plotRef == 'color']
  y <- map$id[map$plotRef == 'yaxis']
  x <- map$id[map$plotRef == 'xaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  series <- noStatsFacet(data, group, panel)
  names(series) <- c(group, panel, 'series.y', 'series.x') 

  if (smoothedMean) {
    interval <- groupSmoothedMean(data, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel) 
    mergeCols <- c(group, panel)
    data <- merge(series, interval, by = mergeCols)
  } else {
    data <- series
  }

  return(data)
}


#' Line Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'series.x' and 'series.y'
#' contain the raw data for the line plot. Column 'group' and 
#' 'panel' specify the group the series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @return data.table of plot-ready data
#' @export
line <- function(data, map) {
  group <- map$id[map$plotRef == 'color']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  data <- noStatsFacet(data, group, panel)

  return(data)
}


#' Density Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'series.x' and 'series.y'
#' contain the calculated kernel density estimates. Column 'group' 
#' and 'panel' specify the group the series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @return data.table of plot-ready data
#' @export
density <- function(data, map) {
  group <- map$id[map$plotRef == 'color']
  x <- map$id[map$plotRef == 'xaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  data <- groupDensity(data, x, group, panel)

  return(data)
}

#' Heatmap as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Column 'table' contains a nested
#' data.table of z-values for plotting. This table has a column for
#' each x-axis entry and a row for each y-axis entry. Column 'group'
#' and 'panel' specify the group the series data belongs to. 
#' There are three ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yaxis = categorical, xaxis = date and zaxis = numeric
#' 3) 'interaction' of two categorical variables (cont. table)
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series', 'interaction')
#' @return data.table of plot-ready data
#' @export
heatmap <- function(data, map, value = 'collection') {
  group <- map$id[map$plotRef == 'color']
  #NOTE: one or the other of these could be a list for 'collection'
  y <- map$id[map$plotRef == 'yaxis']
  x <- map$id[map$plotRef == 'xaxis']
  #NOTE: this for the case of 'series'
  z <- map$id[map$plotRef == 'zaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  if (value == 'collection') {
    data <- groupSplit(data, group, panel)
  } else if (value == 'series' ) {
    data <- data[order(data[[x]]),]
    data[[x]] <- as.factor(data[[x]])
    data[[y]] <- as.factor(data[[y]])
    data <- groupSplit(data, x, y, z, group, panel, longToWide = TRUE)
  } else if (value == 'interaction') {
    data[[x]] <- as.factor(data[[x]])
    data[[y]] <- as.factor(data[[y]])    
    data <- groupTable(data, x, y, group, panel)
  } else {
    stop('Unrecognized argument to "value".')
  }

  return(data)
}

## TODO box plot needs lowerfence and upperfence not min and max

#' Box Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'x', 'min', 'q1', 'median', 
#' 'q3' and 'max' represent the pre-computed values per group. 
#' Column 'group' and 'panel' specify the group the data belong to. 
#' Optionally, can return columns 'outliers', 'mean' and 'sd' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param outliers boolean indicating whether to return a list of outliers per group (per panel)
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param sd boolean indicating whether to return the standard deviation per group (per panel)
#' @return data.table of plot-ready data
#' @export
box <- function(data, map, outliers = FALSE, mean = FALSE, sd = FALSE) {
  group <- map$id[map$plotRef == 'color']
  y <- map$id[map$plotRef == 'yaxis']
  x <- map$id[map$plotRef == 'xaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  data <- groupSummary(data, x, y, group, panel)
  if (outliers) {
    outliers <- groupOutliers(data, x, y, group, panel)
    mergeCols <- c(group, panel)
    data <- merge(data, outliers, by = byCols)
  }

  if (mean) {
    mean <- groupMean(data, x, y, group, panel)
    mergeCols <- c(group, panel)
    data <- merge(data, mean, by = byCols)
  }

  if (sd) {
    sd <- groupSD(data, x, y, group, panel)
    mergeCols <- c(group, panel)
    data <- merge(data, sd, by = byCols)
  }

  return(data)
}


#' Bar Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'label' and 'value'
#' contain the raw data for plotting. Column 'group' 
#' and 'panel' specify the group the series data belongs to.
#' There are two options to calculate y-values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurances of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param value String indicating how to calculate y-values ('identity', 'count')
#' @return data.table of plot-ready data
#' @export
bar <- function(data, map, value = 'identity') {
  group <- map$id[map$plotRef == 'color']
  y <- map$id[map$plotRef == 'yaxis']
  x <- map$id[map$plotRef == 'xaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  if (value == 'identity') {
    data <- noStatsFacet(data, group, panel)
  } else if (value == 'count' ) {
    data <- groupSize(data, x, y, group, panel)
    names(data) <- c(x, group, panel, 'y')
  } else {
    stop('Unrecognized argument to "value".')
  }

  return(data)
}


#' Histogram as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'series.x' and 'series.y'
#' contain the bin label and count respectively. Column 'group' 
#' and 'panel' specify the group the series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param binWidth numeric value indicating width of bins 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @return data.table of plot-ready data
#' @export
histogram <- function(data, map, binWidth = .1, value = 'count') {
  group <- map$id[map$plotRef == 'color']
  x <- map$id[map$plotRef == 'xaxis']
  facet1 <- map$id[map$plotRef == 'facet1']
  facet2 <- map$id[map$plotRef == 'facet2']

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]

  if (value == 'count') {
    data <- binSize(data, x, group, panel, binWidth)
  } else if (value == 'proportion' ) {
    data <- binProportion(data, x, group, panel, binWidth)
  } else {
    stop('Unrecognized argument to "value".')
  }

  return(data)
}

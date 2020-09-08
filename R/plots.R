# TODO make sure column names in 'data' are right ('x', 'y', 'overlayVariable' etc)
# TODO make sure each plot type has all needed map entries
## ex line may need x and y just to relabel the columns ??
# TODO consider whether to convert to JSON in R or Java
## if we do it in R, i need to have one row per panel rather than per group per panel. otherwise we can loop through unique panels in java 

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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  series <- noStatsFacet(data, group, panel)
  names(series) <- c(group, panel, 'series.y', 'series.x') 

  if (smoothedMean) {
    interval <- groupSmoothedMean(data, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel) 
    if (!is.null(mergeByCols)) {
      data <- merge(series, interval, by = mergeByCols)
    } else {
      data <- cbind(series, interval)
    }
  } else {
    data <- series
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  data <- groupDensity(data, x, group, panel)
  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

# TODO possible we wont have groups and panels here.. probably best
# dev tests once we know what we're doing here

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
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' 3) 'interaction' of two categorical variables (cont. table)
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot 
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series', 'interaction')
#' @return data.table of plot-ready data
#' @export
heatmap <- function(data, map, value = 'collection') {
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  #NOTE: one or the other of these could be a list for 'collection'
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  #NOTE: this for the case of 'series'
  z <- emptyStringToNull(map$id[map$plotRef == 'zaxis'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, z, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

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

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x, z), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  data <- groupSummary(data, x, y, group, panel)
  if (outliers) {
    outliers <- groupOutliers(data, x, y, group, panel)
    data <- merge(data, outliers, by = mergeByCols)
  }

  if (mean) {
    mean <- groupMean(data, x, y, group, panel)
    data <- merge(data, mean, by = mergeByCols)
  }

  if (sd) {
    sd <- groupSD(data, x, y, group, panel)
    data <- merge(data, sd, by = mergeByCols)
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

# TODO make sure this can serve pie as well

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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  y <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  if (value == 'identity') {
    data <- noStatsFacet(data, group, panel)
  } else if (value == 'count' ) {
    data <- groupSize(data, x, y, group, panel)
    names(data) <- c(x, group, panel, 'y')
  } else {
    stop('Unrecognized argument to "value".')
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
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
  group <- emptyStringToNull(map$id[map$plotRef == 'overlayVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  if (value == 'count') {
    data <- binSize(data, x, group, panel, binWidth)
  } else if (value == 'proportion' ) {
    data <- binProportion(data, x, group, panel, binWidth)
  } else {
    stop('Unrecognized argument to "value".')
  }

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

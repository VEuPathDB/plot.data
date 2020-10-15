# TODO make sure results are ordered so we can convert to json and stream in Java

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
#' @param smoothedMean boolean indicating whether to calculate smoothed mean
#' @return data.table of plot-ready data
#' @export
scattergl <- function(data, smoothedMean = FALSE) {
  group <- 'overlayVariable'
  y <- 'yAxisVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

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

#' Density Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'series.x' and 'series.y'
#' contain the calculated kernel density estimates. Column 'group' 
#' and 'panel' specify the group the series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @return data.table of plot-ready data
#' @export
density <- function(data) {
  group <- 'overlayVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

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

#' Heatmap as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Column 'table' contains a nested
#' data.table of z-values for plotting. This table has a column for
#' each x-axis entry and a row for each y-axis entry. Column 'group'
#' and 'panel' specify the group the series data belongs to. 
#' There are two ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' @param data data.frame to make plot-ready data for
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @return data.table of plot-ready data
#' @export
heatmap <- function(data, value = 'series') {
  #NOTE: one or the other of these could be a list for 'collection'
  y <- 'yAxisVariable'
  x <- 'xAxisVariable'
  #NOTE: this for the case of 'series'
  z <- 'zAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, z, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

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

  data.back <- noStatsFacet(data.back, group, panel)
  data.back <- data.back[, -c(y, x, z), with = FALSE]
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Box Plot as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'x', 'min', 'q1', 'median', 
#' 'q3' and 'max' represent the pre-computed values per group. 
#' Column 'group' and 'panel' specify the group the data belong to. 
#' Optionally, can return columns 'outliers', 'mean' and 'sd' as well.
#' @param data data.frame to make plot-ready data for
#' @param outliers boolean indicating whether to return a list of outliers per group (per panel)
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @param sd boolean indicating whether to return the standard deviation per group (per panel)
#' @return data.table of plot-ready data
#' @export
box <- function(data, outliers = FALSE, mean = FALSE, sd = FALSE) {
  group <- 'overlayVariable'
  y <- 'yAxisVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(y, x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

  data <- groupSummary(data, x, y, group, panel)
  #data <- data[, -c('min', 'max'), with=FALSE]
  fences <- groupFences(data, x, y, group, panel)
  data <- merge(data, fences, by = mergeByCols)

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
#' @param value String indicating how to calculate y-values ('identity', 'count')
#' @return data.table of plot-ready data
#' @export
bar <- function(data, value = 'identity') {
  group <- 'overlayVariable'
  y <- 'yAxisVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

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
#' row per group (per panel). Columns 'x' and 'y'
#' contain the bin label and count respectively. Column 'group' 
#' and 'panel' specify the group the series data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @return data.table of plot-ready data
#' @export
histogram <- function(data, binWidth = .1, value = 'count') {
  group <- 'overlayVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

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
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Contingency Table as data.table
#'
#' This function returns a data.table of plot-ready data with one
#' row per group (per panel). Columns 'x' and 'y'
#' contain the raw data for plotting. Column 'panel' specifies
#' the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @return data.table of plot-ready data
#' @export
contingecyTable <- function(data) {
  group <- 'overlayVariable'
  x <- 'xAxisVariable'
  facet1 <- 'facetVariable1'
  facet2 <- 'facetVariable2'

  panelData <- makePanels(data, facet1, facet2)
  data <- panelData[[1]]
  panel <- panelData[[2]]
  myCols <- c(x, group, panel)
  data.back <- data
  data <- as.data.table(data)
  data <- data[, myCols, with=FALSE]
  mergeByCols <- c(group, panel)

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
  if (!is.null(mergeByCols)) {
    data <- merge(data, data.back, by = mergeByCols)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

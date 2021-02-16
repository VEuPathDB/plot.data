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

  #TODO is there a reason for valid NA here, should compare counts before and after maybe ?
  xIsNum = all(!is.na(as.numeric(data[[x]])))
  xIsDate = !xIsNum && all(!is.na(as.POSIXct(data[[x]], format='%Y-%m-%d')))
  if (xIsNum) {
    data[[x]] <- as.numeric(data[[x]])
  } else if (xIsDate) {
    data[[x]] <- as.POSIXct(data[[x]], format='%Y-%m-%d')
  } else {
    stop("X axis must be numeric or date.")
  }

  if (value == 'count') {
    data <- binSize(data, x, group, panel, binWidth)
  } else if (value == 'proportion' ) {
    data <- binProportion(data, x, group, panel, binWidth)
  } else {
    stop('Unrecognized argument to "value".')
  }

  #data.back <- noStatsFacet(data.back, group, panel)
  #data.back <- data.back[, -c(x), with = FALSE]
  #if (!is.null(key(data.back))) {
  #  data <- merge(data, data.back)
  #} else {
  #  data <- cbind(data, data.back)
  #}

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
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @return character name of json file containing plot-ready data
#' @export
histogram <- function(data, map, binWidth = NULL, value = c('count', 'proportion'), binReportValue = c('binWidth', 'numBins')) {
  value <- match.arg(value)
  binReportValue <- match.arg(binReportValue)
  dt <- histogram.dt(data, map, binWidth, value)

  if (binReportValue == 'binWidth') {
    if (is.null(binWidth)) {
      binStart <- as.numeric(findBinStart(unlist(dt$binLabel)))
      binEnd <- as.numeric(findBinEnd(unlist(dt$binLabel)))
      binWidth <- getMode(binEnd - binStart) 
    }
    namedAttrList <- list('binWidth' = binWidth)
  } else {
    numBins <- length(unlist(dt$binLabel))
    namedAttrList <- list('numBins' = numBins)
  }

  outFileName <- writeJSON(dt, 'histogram', namedAttrList)

  return(outFileName)
}

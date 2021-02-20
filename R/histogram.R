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
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param viewport List of min and max values to consider as the range of data
#' @return data.table plot-ready data
#' @importFrom stringr str_count
#' @export
histogram.dt <- function(data, map, binWidth, value, binReportValue, viewport) {
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

  incompleteCaseCount <- nrow(data[!complete.cases(data),])
  data <- data[complete.cases(data),]

  if (binReportValue == 'numBins') {
    binSlider <- list('min'=2, 'max'=1000, 'step'=1)
  } else {
    binSliderMax <- (max(data[[x]]) - min(data[[x]])) / 2
    binSliderMin <- (max(data[[x]]) - min(data[[x]])) / 1000
    avgDigits <- floor(mean(stringr::str_count(as.character(data[[x]]), "[[:digit:]]")))
    binSliderMax <- round(binSliderMax, avgDigits)
    binSliderMin <- round(binSliderMin, avgDigits)
    # TODO not sure this is the rule we meant ?
    binSliderStep <- round((binSliderMax / 1000), avgDigits) 
    binSlider <- list('min'=binSliderMin, 'max'=binSliderMax, 'step'=binSliderStep)
  }

  xIsNum = all(!is.na(as.numeric(data[[x]])))
  xIsDate = !xIsNum && all(!is.na(as.POSIXct(data[[x]], format='%Y-%m-%d')))
  if (xIsNum) {
    data[[x]] <- as.numeric(data[[x]])
  } else if (xIsDate) {
    data[[x]] <- as.POSIXct(data[[x]], format='%Y-%m-%d')
  } else {
    stop("X axis must be numeric or date.")
  }

  #TODO consider making viewport a dedicated class of object
  # rather than just assumping its a list w 'min' and 'max' tags
  # particularly if it becomes necessary for types other than histo
  if (is.null(viewport)) {
    viewport <- list('min' = min(0,min(data[[x]])), 'max' = max(data[[x]]))
  } else {
    if (xIsNum) {
      viewport$min <- as.numeric(viewport$min)
      viewport$max <- as.numeric(viewport$max)
    } else if (xIsDate) {
      viewport$min <- as.POSIXct(viewport$min, format='%Y-%m-%d')
      viewport$max <- as.POSIXct(viewport$max, format='%Y-%m-%d')
    }
  }

  if (value == 'count') {
    data <- binSize(data, x, group, panel, binWidth, viewport)
  } else if (value == 'proportion' ) {
    data <- binProportion(data, x, group, panel, binWidth, viewport)
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

  return(list(data, incompleteCaseCount, binSlider))
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
#' @param viewport List of min and max values to consider as the range of data
#' @return character name of json file containing plot-ready data
#' @export
histogram <- function(data, map, binWidth = NULL, value = c('count', 'proportion'), binReportValue = c('binWidth', 'numBins'), viewport = NULL) {
  value <- match.arg(value)
  binReportValue <- match.arg(binReportValue)
  outList <- histogram.dt(data, map, binWidth, value, binReportValue, viewport)
  dt <- outList[[1]]
  namedAttrList <- list('incompleteCases' = outList[[2]], 'binSlider' = outList[[3]])

  if (binReportValue == 'binWidth') {
    if (is.null(binWidth)) {
      binStart <- as.numeric(findBinStart(unlist(dt$binLabel)))
      binEnd <- as.numeric(findBinEnd(unlist(dt$binLabel)))
      binWidth <- getMode(binEnd - binStart) 
    }
    namedAttrList$binWidth <- binWidth
  } else {
    numBins <- length(unlist(dt$binLabel))
    namedAttrList$numBins <- numBins
  }

  outFileName <- writeJSON(dt, 'histogram', namedAttrList)

  return(outFileName)
}

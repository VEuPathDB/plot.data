#TODO helper functions for grabbing histo specific attrs
newHistogramPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         viewport = list('min.x' = NULL,
                                         'max.x' = NULL),
                         binWidth,
                         binReportValue = character(),
                         value = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     viewport = viewport,
                     class = "histogram")

  attr <- attributes(.pd)
  viewport <- attr(.pd, 'viewport')

  x <- attr$xAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)
  
  #this makes sense for histo bc there are no other stats calculated
  # do NOT do this for other plot types, figure something else out
  .pd <- subset(.pd, .pd[[x]] <= viewport$x.max & .pd[[x]] >= viewport$x.min)

  if (binReportValue == 'numBins') {
    binSlider <- list('min'=jsonlite::unbox(2), 'max'=jsonlite::unbox(1000), 'step'=jsonlite::unbox(1))
  } else {
    binSliderMax <- (max(.pd[[x]]) - min(.pd[[x]])) / 2
    binSliderMin <- (max(.pd[[x]]) - min(.pd[[x]])) / 1000
    avgDigits <- floor(mean(stringr::str_count(as.character(.pd[[x]]), "[[:digit:]]")))
    binSliderMax <- round(binSliderMax, avgDigits)
    binSliderMin <- round(binSliderMin, avgDigits)
    # TODO not sure this is the rule we meant ?
    binSliderStep <- round((binSliderMax / 1000), avgDigits)
    binSlider <- list('min'=jsonlite::unbox(binSliderMin), 'max'=jsonlite::unbox(binSliderMax), 'step'=jsonlite::unbox(binSliderStep))
  }
  attr$binSlider <- binSlider

  #TODO since viewport is a .pd attr, dont need to pass it
  # binWidth as well
  if (value == 'count') {
    .pd <- binSize(.pd, x, group, panel, binWidth, viewport)
    attr$names <- names(.pd)
  } else if (value == 'proportion' ) {
    .pd <- binProportion(.pd, x, group, panel, binWidth, viewport)
    attr$names <- names(.pd)
  } else {
    stop('Unrecognized argument to "value".')
  }

  if (binReportValue == 'binWidth') {
    if (is.null(binWidth)) {
      binStart <- as.numeric(findBinStart(unlist(.pd$binLabel)))
      binEnd <- as.numeric(findBinEnd(unlist(.pd$binLabel)))
      binWidth <- getMode(binEnd - binStart)
    }
    attr$binWidth <- jsonlite::unbox(binWidth)
  } else {
    numBins <- length(unlist(dt$binLabel))
    attr$numBins <- jsonlite::unbox(numBins)
  }

  attributes(.pd) <- attr

  return(.pd)
}

validateBinSlider <- function(binSlider) {
  if (!is.list(binSlider)) {
    return(FALSE)
  } else{
    if (!all(c('max', 'min', 'step') %in% names(binSlider))) {
      return(FALSE)
    }
  }

  return(TRUE)
}

validateHistogramPD <- function(.histo) {
  binSlider <- attr(.histo, 'binSlider')
  stopifnot(validateBinSlider(binSlider))
  xAxisVariable <- attr(.histo, 'xAxisVariable')
  if (!xAxisVariable$dataType %in% c('DATE','NUMBER')) {
    stop('The independent axis must be either of type date or number for a histogram.')
  }
  binWidth <- attr(.histo, 'binWidth')
  if (xAxisVariable$dataType == 'DATE' && !is.character(binWidth)) {
    stop("binWidth must be a character string for histograms of date values.")
  } else if (xAxisVariable$dataType == 'NUMBER' && !is.numeric(binWidth)) {
    stop("binWidth must be numeric for histograms of numeric values.")
  }

  return(.histo)
}

#' Histogram as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param viewport List of min and max values to consider as the range of data
#' @return data.table plot-ready data
#' @importFrom stringr str_count
#' @importFrom jsonlite unbox
#' @export
histogram.dt <- function(data, 
                         map, 
                         binWidth, 
                         value, 
                         binReportValue, 
                         viewport) {

  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)

  if (!'data.table' %in% class(data)) {
    data <- data.table::as.data.table(data)
  }

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- list('variableId' = map$id[map$plotRef == 'xAxisVariable'],
                          'entityId' = map$entityId[map$plotRef == 'xAxisVariable'],
                          'dataType' = map$dataType[map$plotRef == 'xAxisVariable'])
    if (xAxisVariable$dataType == 'NUMBER' & !is.null(binWidth)) {
      binWidth <- suppressWarnings(as.numeric(binWidth))
      if (is.na(binWidth)) {
        stop("binWidth must be numeric for histograms of numeric values.")
      }
    }
  } else {
    stop("Must provide xAxisVariable for plot type histogram.")
  }
  if ('overlayVariable' %in% map$plotRef) {
    overlayVariable <- list('variableId' = map$id[map$plotRef == 'overlayVariable'],
                            'entityId' = map$entityId[map$plotRef == 'overlayVariable'],
                            'dataType' = map$dataType[map$plotRef == 'overlayVariable'])
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- list('variableId' = map$id[map$plotRef == 'facetVariable1'],
                           'entityId' = map$entityId[map$plotRef == 'facetVariable1'],
                           'dataType' = map$dataType[map$plotRef == 'facetVariable1'])
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- list('variableId' = map$id[map$plotRef == 'facetVariable2'],
                           'entityId' = map$entityId[map$plotRef == 'facetVariable2'],
                           'dataType' = map$dataType[map$plotRef == 'facetVariable2'])
  }

  .histo <- newHistogramPD(.dt = data,
                           xAxisVariable = xAxisVariable,
                           overlayVariable = overlayVariable,
                           facetVariable1 = facetVariable1,
                           facetVariable2 = facetVariable2,
                           viewport = viewport,
                           binWidth,
                           binReportValue,
                           value,
                           class = "histogram")

  .histo <- validateHistogramPD(.histo)

  return(.histo)
}

#' Histogram data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param viewport List of min and max values to consider as the range of data
#' @return character name of json file containing plot-ready data
#' @importFrom jsonlite unbox
#' @export
histogram <- function(data, 
                      map, 
                      binWidth = NULL, 
                      value = c('count', 'proportion'), 
                      binReportValue = c('binWidth', 'numBins'), 
                      viewport = NULL) {

  value <- match.arg(value)
  binReportValue <- match.arg(binReportValue)

  .histo <- histogram.dt(data, map, binWidth, value, binReportValue, viewport)
  outFileName <- writeJSON(.histo, 'histogram')

  return(outFileName)
}

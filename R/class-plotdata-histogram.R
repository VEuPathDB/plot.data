#' @importFrom zoo as.yearmon
newHistogramPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         viewport = list('xMin' = NULL,
                                         'xMax' = NULL),
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
                     class = "histogram")

  attr <- attributes(.pd)
  x <- attr$xAxisVariable$variableId
  xType <- attr$xAxisVariable$dataType
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  summary <- as.list(summary(.pd[[x]]))
  names(summary) <- c('min', 'q1', 'median', 'mean', 'q3', 'max')
  summary <- lapply(summary, as.character)
  summary <- lapply(summary, jsonlite::unbox)
  attr$summary <- summary

  if (is.null(viewport)) {
    if (xType == 'NUMBER') {
      viewport <- list('xMin' = min(0,min(.pd[[x]])), 'xMax' = max(.pd[[x]]))
    } else {
      viewport <- list('xMin' = min(.pd[[x]]), 'xMax' = max(.pd[[x]]))
    }
  } else {
    if (xType == 'NUMBER') {
      viewport$xMin <- as.numeric(viewport$xMin)
      viewport$xMax <- as.numeric(viewport$xMax)
    } else if (xType == 'DATE') {
      viewport$xMin <- as.Date(viewport$xMin, format='%Y-%m-%d')
      viewport$xMax <- as.Date(viewport$xMax, format='%Y-%m-%d')
    }
  }
  attr$viewport <- lapply(viewport, as.character)
  attr$viewport <- lapply(attr$viewport, jsonlite::unbox)
  xVP <- adjustToViewport(.pd[[x]], viewport)

  if (binReportValue == 'binWidth') {
    if (is.null(binWidth)) {
      binWidth <- findBinWidth(xVP)
    }
  } else {
    numBins <- findNumBins(xVP)
  }

  if (binReportValue == 'numBins') {
    binSlider <- list('min'=jsonlite::unbox(2), 'max'=jsonlite::unbox(1000), 'step'=jsonlite::unbox(1))
    binSpec <- list('type'=jsonlite::unbox('numBins'), 'value'=jsonlite::unbox(numBins))
  } else {
    binSliderMax <- as.numeric((max(xVP) - min(xVP)) / 2)
    binSliderMin <- as.numeric((max(xVP) - min(xVP)) / 1000)
    if (xType == 'NUMBER') {
      avgDigits <- floor(mean(stringr::str_count(as.character(xVP), "[[:digit:]]")))
      binSliderMax <- round(binSliderMax, avgDigits)
      binSliderMin <- round(binSliderMin, avgDigits)
      binSliderStep <- round(((binSliderMax - binSliderMin) / 1000), avgDigits)
      binSliderMin <- ifelse(binSliderMin == 0, .1, binSliderMin)
      binSliderStep <- ifelse(binSliderStep == 0, binSliderMin, binSliderStep)
      binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(binWidth))
    } else {
      if (is.null(binWidth)) {
        binWidth <- findBinWidth(xVP)
      }
      numericBinWidth <- as.numeric(gsub("[^0-9.-]", "", binWidth))
      if (is.na(numericBinWidth)) { numericBinWidth <- 1 }
      unit <- trim(gsub("^[[:digit:]].", "", binWidth))
      if (unit %in% c('day', 'days')) {
        binSliderMin <- floor(binSliderMin)
        binSliderMax <- ceiling(binSliderMax)
      } else if (unit %in% c('week', 'weeks')) {
        numWeeks <- floor(as.numeric(difftime(max(xVP), min(xVP), units='weeks'))) 
        binSliderMin <- floor(numWeeks/1000)
        binSliderMax <- ceiling(numWeeks/2)
      } else if (unit %in% c('month', 'months')) {
        numMonths <- uniqueN(zoo::as.yearmon(xVP))
        binSliderMin <- floor(numMonths/1000)
        binSliderMax <- ceiling(numMonths/2) 
      } else if (unit %in% c('year', 'years')) {
        numYears <- uniqueN(strSplit(as.character(xVP), '-', 3))
        binSliderMin <- floor(numYears/1000)
        binSliderMax <- ceiling(numYears/2)
      } else {
        stop("Unrecognized unit for date histogram.")
      }
      binSliderMin <- ifelse(binSliderMin == 0, 1, binSliderMin)
      binSliderStep <- 1
      binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(numericBinWidth), 'units'=jsonlite::unbox(unit))
    }
    binSlider <- list('min'=jsonlite::unbox(binSliderMin), 'max'=jsonlite::unbox(binSliderMax), 'step'=jsonlite::unbox(binSliderStep))
  }
  attr$binSlider <- binSlider
  attr$binSpec <- binSpec

  if (value == 'count') {
    .pd <- binSize(.pd, x, group, panel, binWidth, viewport)
  } else if (value == 'proportion' ) {
    .pd <- binProportion(.pd, x, group, panel, binWidth, viewport)
  } else {
    stop('Unrecognized argument to "value".')
  }
  
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

binSlider <- function(.histo) { attr(.histo, 'binSlider') }
viewport <- function(.histo) { attr(.histo, 'viewport') }
binWidth <- function(.histo) { ifelse(attr(.histo, 'binSpec')$type == 'binWidth', attr(.histo, 'binSpec')$value, NULL) }
numBins <- function(.histo) { ifelse(attr(.histo, 'binSpec')$type == 'numBins', attr(.histo, 'binSpec')$value, NULL) }

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

validateViewport <- function(viewport) {
  if (!is.list(viewport)) {
    return(FALSE)
  } else{
    if (!all(c('xMax', 'xMin') %in% names(viewport))) {
      return(FALSE)
    }
  }

  return(TRUE)
}

validateHistogramPD <- function(.histo) {
  binSlider <- attr(.histo, 'binSlider')
  stopifnot(validateBinSlider(binSlider))
  viewport <- attr(.histo, 'viewport')
  stopifnot(validateViewport(viewport))
  xAxisVariable <- attr(.histo, 'xAxisVariable')
  if (!xAxisVariable$dataShape == 'CONTINUOUS') {
    stop('The independent axis must be continuous for a histogram.')
  }
  overlayVariable <- attr(.histo, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The overlay variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable1 <- attr(.histo, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.histo, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }
  binWidth <- attr(.histo, 'binWidth')
  if (!is.null(binWidth)) {
    if (xAxisVariable$dataType == 'DATE' && !is.character(binWidth)) {
      stop("binWidth must be a character string for histograms of date values.")
    } else if (xAxisVariable$dataType == 'NUMBER' && !is.numeric(binWidth)) {
      stop("binWidth must be numeric for histograms of numeric values.")
    }
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
                         binWidth = NULL, 
                         value = c('count', 'proportion'), 
                         binReportValue = c('binWidth', 'numBins'), 
                         viewport = NULL) {

  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL,
                         'dataShape' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL,
                        'dataShape' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL,
                        'dataShape' = NULL)
  value <- matchArg(value)
  binReportValue <- matchArg(binReportValue)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
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
    overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }

  .histo <- newHistogramPD(.dt = data,
                           xAxisVariable = xAxisVariable,
                           overlayVariable = overlayVariable,
                           facetVariable1 = facetVariable1,
                           facetVariable2 = facetVariable2,
                           viewport = viewport,
                           binWidth = binWidth,
                           binReportValue = binReportValue,
                           value = value)

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

  value <- matchArg(value)
  binReportValue <- matchArg(binReportValue)

  .histo <- histogram.dt(data, map, binWidth, value, binReportValue, viewport)
  outFileName <- writeJSON(.histo, 'histogram')

  return(outFileName)
}

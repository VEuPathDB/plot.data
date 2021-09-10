#' @importFrom zoo as.yearmon
newHistogramPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         viewport = list('xMin' = NULL,
                                         'xMax' = NULL),
                         binWidth,
                         binReportValue = character(),
                         value = character(),
                         barmode = character(),
                         evilMode = logical(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "histogram")

  attr <- attributes(.pd)
  x <- toColNameOrNull(attr$xAxisVariable)
  xType <- attr$xAxisVariable$dataType
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  summary <- as.list(summary(.pd[[x]]))
  names(summary) <- c('min', 'q1', 'median', 'mean', 'q3', 'max')
  summary <- lapply(summary, as.character)
  summary <- lapply(summary, jsonlite::unbox)
  attr$summary <- summary
  logWithTime('Supporting summary statistics calculated for histogram.', verbose)

  if (is.null(viewport)) {
    if (xType == 'NUMBER') {
      viewport <- list('xMin' = min(0,min(.pd[[x]])), 'xMax' = max(.pd[[x]]))
    } else {
      viewport <- list('xMin' = min(.pd[[x]]), 'xMax' = max(.pd[[x]]))
    }
    logWithTime('Determined default viewport.', verbose)
  } else {
    if (xType == 'NUMBER') {
      viewport$xMin <- as.numeric(viewport$xMin)
      viewport$xMax <- as.numeric(viewport$xMax)
    } else if (xType == 'DATE') {
      viewport$xMin <- as.Date(viewport$xMin, format='%Y-%m-%d')
      viewport$xMax <- as.Date(viewport$xMax, format='%Y-%m-%d')
    }
    logWithTime('Using provided viewport.', verbose)
  }
  attr$viewport <- lapply(viewport, as.character)
  attr$viewport <- lapply(attr$viewport, jsonlite::unbox)
  xVP <- adjustToViewport(.pd[[x]], viewport)

  if (binReportValue == 'binWidth') {
    if (is.null(binWidth)) {
      binWidth <- findBinWidth(xVP)
      logWithTime('Determined ideal bin width.', verbose)
    }
  } else {
    if (is.null(binWidth)) {
      numBins <- findNumBins(xVP)
      logWithTime('Determined ideal number of bins.', verbose)
    } else {
      numBins <- binWidthToNumBins(xVP, binWidth)
      logWithTime('Converted provided bin width to number of bins.', verbose)
    }
  }

  if (binReportValue == 'numBins') {
    binSlider <- list('min'=jsonlite::unbox(2), 'max'=jsonlite::unbox(1000), 'step'=jsonlite::unbox(1))
    binSpec <- list('type'=jsonlite::unbox('numBins'), 'value'=jsonlite::unbox(numBins))
  } else {
    binSliderMax <- as.numeric((max(xVP) - min(xVP)) / 2)
    binSliderMin <- as.numeric((max(xVP) - min(xVP)) / 1000)
    if (xType == 'NUMBER') {
      avgDigits <- floor(mean(stringi::stri_count_regex(as.character(xVP), "[[:digit:]]")))
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
  logWithTime('Determined bin width slider min, max and step values.', verbose)

  if (value == 'count') {
    .pd <- binSize(.pd, x, group, panel, binWidth, viewport)
    logWithTime('Value is set to `count`. Resulting histogram object will represent counts of unique x-axis bins per group.', verbose)
  } else if (value == 'proportion' ) {
    .pd <- binProportion(.pd, x, group, panel, binWidth, barmode, viewport)
    logWithTime('Value is set to `proportion`. If barmode is `group` the resulting histogram object will represent the relative proportions of unique x-axis bins across groups. If barmode is `stack` the resulting histogram object will represent the proportions of unique x-axis bins relative to the total x-axis bins in that panel.', verbose)
  } else {
    stop('Unrecognized argument to "value".')
  }
  
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

binSlider <- function(.histo) { attr(.histo, 'binSlider') }
binSpec <- function(.histo) { attr(.histo, 'binSpec') }
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

validateHistogramPD <- function(.histo, verbose) {
  binSlider <- attr(.histo, 'binSlider')
  stopifnot(validateBinSlider(binSlider))
  viewport <- attr(.histo, 'viewport')
  stopifnot(validateViewport(viewport))
#  xAxisVariable <- attr(.histo, 'xAxisVariable')
#  if (!xAxisVariable$dataShape == 'CONTINUOUS') {
#    stop('The independent axis must be continuous for a histogram.')
#  }
#  overlayVariable <- attr(.histo, 'overlayVariable')
#  if (!is.null(overlayVariable)) {
#    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The overlay variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable1 <- attr(.histo, 'facetVariable1')
#  if (!is.null(facetVariable1)) {
#    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The first facet variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable2 <- attr(.histo, 'facetVariable2')
#  if (!is.null(facetVariable2)) {
#    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The second facet variable must be binary, ordinal or categorical.')
#    }
#  }
  binWidth <- attr(.histo, 'binWidth')
  if (!is.null(binWidth)) {
    if (xAxisVariable$dataType == 'DATE' && !is.character(binWidth)) {
      stop("binWidth must be a character string for histograms of date values.")
    } else if (xAxisVariable$dataType == 'NUMBER' && !is.numeric(binWidth)) {
      stop("binWidth must be numeric for histograms of numeric values.")
    }
  }
  logWithTime('Histogram request has been validated!', verbose)
  
  return(.histo)
}

#' Histogram as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. It is possible to plot missingness in the stratification variables as an explicit 'No data' value using `evilMode`.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param barmode String indicating if bars should be stacked or overlaid ('stack', 'overlay')
#' @param viewport List of min and max values to consider as the range of data
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @importFrom stringi stri_count_regex
#' @importFrom jsonlite unbox
#' @export
histogram.dt <- function(data, 
                         map, 
                         binWidth = NULL, 
                         value = c('count', 'proportion'), 
                         binReportValue = c('binWidth', 'numBins'),
                         barmode = c('stack', 'overlay'),
                         viewport = NULL,
                         evilMode = c(FALSE, TRUE),
                         verbose = c(TRUE, FALSE)) {

  value <- matchArg(value)
  barmode <- matchArg(barmode)
  binReportValue <- matchArg(binReportValue)
  evilMode <- matchArg(evilMode)
  verbose <- matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type histogram.")
  } else {
    if (xAxisVariable$dataType == 'NUMBER' & !is.null(binWidth)) {
      binWidth <- suppressWarnings(as.numeric(binWidth))
      if (is.na(binWidth)) {
        stop("binWidth must be numeric for histograms of numeric values.")
      }
    }
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .histo <- newHistogramPD(.dt = data,
                           xAxisVariable = xAxisVariable,
                           overlayVariable = overlayVariable,
                           facetVariable1 = facetVariable1,
                           facetVariable2 = facetVariable2,
                           viewport = viewport,
                           binWidth = binWidth,
                           binReportValue = binReportValue,
                           value = value,
                           barmode = barmode,
                           evilMode = evilMode,
                           verbose = verbose)

  .histo <- validateHistogramPD(.histo, verbose)
  logWithTime(paste('New histogram object created with parameters viewport min =', viewport$xMin, ', viewport max =', viewport$xMax, ', binWidth =', binWidth, ', binReportValue =', binReportValue, ', value =', value, ', barmode =', barmode, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.histo)
}

#' Histogram data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param barmode String indicating if bars should be stacked or overlaid ('stack', 'overlay')
#' @param viewport List of min and max values to consider as the range of data
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @importFrom jsonlite unbox
#' @export
histogram <- function(data, 
                      map, 
                      binWidth = NULL, 
                      value = c('count', 'proportion'), 
                      binReportValue = c('binWidth', 'numBins'), 
                      barmode = c('stack', 'overlay'),
                      viewport = NULL,
                      evilMode = c(FALSE, TRUE),
                      verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .histo <- histogram.dt(data, map, binWidth, value, binReportValue, barmode, viewport, evilMode, verbose)
  outFileName <- writeJSON(.histo, evilMode, 'histogram', verbose)

  return(outFileName)
}

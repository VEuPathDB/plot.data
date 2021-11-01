newMosaicPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         yAxisVariable = list('variableId' = NULL,
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
                         statistic = character(),
                         evilMode = logical(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "mosaic")

  attr <- attributes(.pd)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  if (statistic == 'chiSq') {
    statsTable <- panelChiSq(.pd, x, y, panel)
    veupathUtils::logWithTime('Calculated chi-squared statistic.', verbose)
  } else {
    statsTable <- panelBothRatios(.pd, x, y, panel)
    veupathUtils::logWithTime('Calculated odds ratio and relative risk.', verbose)
  }
  .pd <- panelTable(.pd, x, y, panel)

  attr$names <- names(.pd)
  attr$statsTable <- statsTable

  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateMosaicPD <- function(.mosaic, verbose) {
#  xAxisVariable <- attr(.mosaic, 'xAxisVariable')
#  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#    stop('The independent axis must be binary, ordinal or categorical for mosaic.')
#  }
#  yAxisVariable <- attr(.mosaic, 'yAxisVariable')
#  if (!yAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#    stop('The dependent axis must be binary, ordinal or categorical for mosaic.')
#  }
#  facetVariable1 <- attr(.mosaic, 'facetVariable1')
#  if (!is.null(facetVariable1)) {
#    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The first facet variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable2 <- attr(.mosaic, 'facetVariable2')
#  if (!is.null(facetVariable2)) {
#    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The second facet variable must be binary, ordinal or categorical.')
#    }
#  }
  veupathUtils::logWithTime('Mosaic plot request has been validated!', verbose)

  return(.mosaic)
}


#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'bothRatios', the second of which will return odds ratios and relative risk.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @export
mosaic.dt <- function(data, map, 
                      statistic = NULL, 
                      evilMode = c(FALSE, TRUE),
                      verbose = c(TRUE, FALSE)) {
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type mosaic.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    stop("Must provide yAxisVariable for plot type mosaic.")
  }

  x <- toColNameOrNull(xAxisVariable)
  y <- toColNameOrNull(yAxisVariable)

  if (!is.null(statistic)) {
    if (!statistic %in% c('chiSq','bothRatios')) {
      stop('`statistic` argument must be one of either \'chiSq\' or \'bothRatios\', the second of which returns both odds ratios and relative risk.')
    }
    #na.rm should be safe, since x and y axes will later have NA removed anyhow in the plot.data parent class
    if ((data.table::uniqueN(data[[x]], na.rm = TRUE) > 2 || data.table::uniqueN(data[[y]], na.rm = TRUE) > 2) && statistic == 'bothRatios') {
      stop('Odds ratio and relative risk can only be calculated for 2x2 contingency tables. Please use statistic `chiSq` instead.')
    }
  } else {
    if (data.table::uniqueN(data[[x]], na.rm = TRUE) > 2 || data.table::uniqueN(data[[y]], na.rm = TRUE) > 2) {
      statistic <- 'chiSq'
    } else {
      statistic <- 'bothRatios'
    }
    veupathUtils::logWithTime(paste('No statistic specified, using:', ifelse(statistic=='chiSq', 'chi-squared', 'odds ratio and relative risk')), verbose)
  }
  
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .mosaic <- newMosaicPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            statistic = statistic,
                            evilMode = evilMode,
                            verbose = verbose)

  .mosaic <- validateMosaicPD(.mosaic, verbose)
  veupathUtils::logWithTime(paste('New mosaic plot object created with parameters statistic =', statistic, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.mosaic)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'bothRatios', the second of which will return odds ratios and relative risk.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @export
mosaic <- function(data, map, 
                   statistic = NULL, 
                   evilMode = c(FALSE, TRUE),
                   verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  .mosaic <- mosaic.dt(data, map, statistic, evilMode, verbose)
  outFileName <- writeJSON(.mosaic, evilMode, 'mosaic', verbose)

  return(outFileName)
}

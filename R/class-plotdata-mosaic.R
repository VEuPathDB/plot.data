newMosaicPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         yAxisVariable = list('variableId' = NULL,
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
                         statistic = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "mosaic")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  if (statistic == 'chiSq') {
    statsTable <- panelChiSq(.pd, x, y, panel)
  } else {
    statsTable <- panelBothRatios(.pd, x, y, panel)
  }
  .pd <- panelTable(.pd, x, y, panel)

  attr$names <- names(.pd)
  attr$statsTable <- statsTable

  setAttrFromList(.pd, attr)

  return(.pd)
}

validateMosaicPD <- function(.mosaic) {
  xAxisVariable <- attr(.mosaic, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
    stop('The independent axis must be binary, ordinal or categorical for mosaic.')
  }
  yAxisVariable <- attr(.mosaic, 'yAxisVariable')
  if (!yAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
    stop('The dependent axis must be binary, ordinal or categorical for mosaic.')
  }
  facetVariable1 <- attr(.mosaic, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.mosaic, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }

  return(.mosaic)
}


#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'bothRatios', the second of which will return odds ratios and relative risk.
#' @return data.table plot-ready data
#' @export
mosaic.dt <- function(data, map, statistic = NULL) {
  yAxisVariable = list('variableId' = NULL,
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

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  } else {
    stop("Must provide xAxisVariable for plot type mosaic.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  } else {
    stop("Must provide yAxisVariable for plot type mosaic.")
  }

  if (!is.null(statistic)) {
    if (!statistic %in% c('chiSq','bothRatios')) {
      stop('`statistic` argument must be one of either \'chiSq\' or \'bothRatios\', the second of which returns both odds ratios and relative risk.')
    }
    if ((data.table::uniqueN(data[[xAxisVariable$variableId]]) > 2 || data.table::uniqueN(data[[yAxisVariable$variableId]]) > 2) && statistic == 'bothRatios') {
      stop('Odds ratio and relative risk can only be calculated for 2x2 contingency tables. Please use statistic `chiSq` instead.')
    }
  } else {
    if (data.table::uniqueN(data[[xAxisVariable$variableId]]) > 2 || data.table::uniqueN(data[[yAxisVariable$variableId]]) > 2) {
      statistic <- 'chiSq'
    } else {
      statistic <- 'bothRatios'
    }
  }
  
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }

  .mosaic <- newMosaicPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            statistic)

  .mosaic <- validateMosaicPD(.mosaic)

  return(.mosaic)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'bothRatios', the second of which will return odds ratios and relative risk.
#' @return character name of json file containing plot-ready data
#' @export
mosaic <- function(data, map, statistic = NULL) {
  .mosaic <- mosaic.dt(data, map, statistic)

  outFileName <- writeJSON(.mosaic, 'mosaic')

  return(outFileName)
}

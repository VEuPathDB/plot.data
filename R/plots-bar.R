newBarPD <- function(.dt = data.table::data.table(),
                         independentVar = list('variableId' = NULL,
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
                         value = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     independentVar = independentVar,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "barplot")

  attr <- attributes(.pd)

  independent <- attr$independentVar$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  if (value == 'identity') {
    .pd <- noStatsFacet(.pd, group, panel)
  } else if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, independent, 'dummy', group, panel)
    names(.pd) <- c('label', group, panel, 'value')
    .pd <- noStatsFacet(.pd, group, panel)
  }
  attr$names <- names(.pd)
  
  attributes(.pd) <- attr

  return(.pd)
}

validateBarPD <- function(.bar) {
  independentVar <- attr(.bar, 'independentVar')
  if (!independentVar$dataType %in% c('STRING')) {
    stop('The independent axis must be of type string for barplot.')
  }

  return(.bar)
}

#' Bar Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are two options to calculate dependent-axis values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurances of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate dependent-axis values ('identity', 'count')
#' @return data.table plot-ready data
#' @export
bar.dt <- function(data, map, value = c('count', 'identity')) {
  value <- match.arg(value)

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

  if ('independentVar' %in% map$plotRef) {
    independentVar <- plotRefMapToList(map, 'independentVar')
  } else {
    stop("Must provide independentVar for plot type bar.")
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

  .bar <- newBarPD(.dt = data,
                    independentVar = independentVar,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    value)

  .bar <- validateBarPD(.bar)

  return(.bar)
}

#' Bar Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are two options to calculate dependent-axis values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurances of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate dependent-axis values ('identity', 'count')
#' @return character name of json file containing plot-ready data
#' @export
bar <- function(data, map, value = c('count', 'identity')) {
  value <- match.arg(value)
  .bar <- bar.dt(data, map, value)
  outFileName <- writeJSON(.bar, 'barplot')

  return(outFileName)
}

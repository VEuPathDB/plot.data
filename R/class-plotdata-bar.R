newBarPD <- function(.dt = data.table::data.table(),
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
                         value = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "barplot")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)
  .pd[[x]] <- as.character(.pd[[x]])

  if (value == 'identity') {
    .pd <- collapseByGroup(.pd, group, panel)
  } else if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, x, 'dummy', group, panel, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))

  } else if (value == 'proportion') {
    .pd$dummy <- 1
    .pd <- groupProportion(.pd, x, 'dummy', group, panel, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))
  }
  attr$names <- names(.pd)
  
  setAttrFromList(.pd, attr)

  return(.pd)
}

validateBarPD <- function(.bar) {
  xAxisVariable <- attr(.bar, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
    stop('The independent axis must be binary, ordinal or categorical for barplot.')
  }
  overlayVariable <- attr(.bar, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The overlay variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable1 <- attr(.bar, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.bar, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }

  return(.bar)
}

#' Bar Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are three options to calculate y-values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurrences of values from data.table input 
#' 3) 'proportion' of occurrences of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @return data.table plot-ready data
#' @export
bar.dt <- function(data, map, value = c('count', 'identity', 'proportion')) {
  value <- matchArg(value)

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

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  } else {
    stop("Must provide xAxisVariable for plot type bar.")
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
                    xAxisVariable = xAxisVariable,
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
#' There are three options to calculate y-values for plotting.
#' 1) raw 'identity' of values from data.table input
#' 2) 'count' occurrences of values from data.table input 
#' 3) 'proportion' of occurrences of values from data.table input 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @return character name of json file containing plot-ready data
#' @export
bar <- function(data, map, value = c('count', 'identity', 'proportion')) {
  value <- matchArg(value)
  .bar <- bar.dt(data, map, value)
  outFileName <- writeJSON(.bar, 'barplot')

  return(outFileName)
}

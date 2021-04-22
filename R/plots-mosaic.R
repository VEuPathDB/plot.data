newMosaicPD <- function(.dt = data.table::data.table(),
                         independentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         dependentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     independentVar = independentVar,
                     dependentVar = dependentVar,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "mosaic")

  attr <- attributes(.pd)

  independent <- attr$independentVar$variableId
  dependent <- attr$dependentVar$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  dims <- as.data.frame.matrix(table(.pd[[independent]], .pd[[dependent]]))
  dims <- c(length(dims), nrow(dims))

  if (any(dims > 2)) {
    .pd <- panelChiSq(.pd, independent, dependent, panel)
  } else {
    .pd <- panelBothRatios(.pd, independent, dependent, panel)
  }
  attr$names <- names(.pd)

  attributes(.pd) <- attr

  return(.pd)
}

validateMosaicPD <- function(.mosaic) {
  independentVar <- attr(.mosaic, 'independentVar')
  if (!independentVar$dataType %in% c('STRING')) {
    stop('The independent axis must be of type string for mosaicplot.')
  }
  dependentVar <- attr(.mosaic, 'dependentVar')
  if (!dependentVar$dataType %in% c('STRING')) {
    stop('The independent axis must be of type string for mosaicplot.')
  }

  return(.mosaic)
}

#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'independent' and 'dependent' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'facetVariable1' and 'facetVariable2'
#' @return data.table plot-ready data
#' @export
mosaic.dt <- function(data, map) {
  dependentVar = list('variableId' = NULL,
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
    stop("Must provide independentVar for plot type mosaic.")
  }
  if ('dependentVar' %in% map$plotRef) {
    dependentVar <- plotRefMapToList(map, 'dependentVar')
  } else {
    stop("Must provide dependentVar for plot type mosaic.")
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }

  .mosaic <- newMosaicPD(.dt = data,
                            independentVar = independentVar,
                            dependentVar = dependentVar,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value)

  .mosaic <- validateMosaicPD(.mosaic)

  return(.mosaic)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'independent' and 'dependent' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'facetVariable1' and 'facetVariable2'
#' @return character name of json file containing plot-ready data
#' @export
mosaic <- function(data, map) {
  .mosaic <- mosaic.dt(data, map)
  outFileName <- writeJSON(.mosaic, 'mosaic')

  return(outFileName)
}

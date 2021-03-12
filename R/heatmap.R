#TODO NOT WORKING, but not priority for now

newHeatmapPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         zAxisVariable = list('variableId' = NULL,
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
                     xAxisVariable = xAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "heatmapplot")

  attr <- attributes(.pd)
  attr$yAxisVariable <- yAxisVariable
  attr$zAxisVariable <- zAxisVariable

  #NOTE: one or the other of these could be a list for 'collection'
  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  #NOTE: this for the case of 'series'
  z <- attr$zAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  if (value == 'collection') {
    data <- groupSplit(data, x, y, NULL, NULL, panel)
  } else if (value == 'series' ) { 
    data <- data[order(data[[x]]),]
    data[[x]] <- as.factor(data[[x]])
    data[[y]] <- as.factor(data[[y]])
    data <- groupSplit(data, x, y, z, NULL, panel, longToWide = TRUE)
  } else {
    stop('Unrecognized argument to "value".')
  } 
  attr$names <- names(.pd)

  attributes(.pd) <- attr

  return(.pd)
}

#TODO figure how to validate x and y axes
validateHeatmapPD <- function(.heatmap) {
  zAxisVariable <- attr(.heatmap, 'zAxisVariable')
  if (!zAxisVariable$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number or date for heatmapplot.')
  }

  return(.heatmap)
}

#' Heatmap as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @return data.table plot-ready data
#' @export
heatmap.dt <- function(data, map, value) {
  zAxisVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
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
  } else {
    stop("Must provide xAxisVariable for plot type scatter.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- list('variableId' = map$id[map$plotRef == 'yAxisVariable'],
                          'entityId' = map$entityId[map$plotRef == 'yAxisVariable'],
                          'dataType' = map$dataType[map$plotRef == 'yAxisVariable'])
  } else {
    stop("Must provide yAxisVariable for plot type scatter.")
  }
  if ('zAxisVariable' %in% map$plotRef) {
    zAxisVariable <- list('variableId' = map$id[map$plotRef == 'zAxisVariable'],
                          'entityId' = map$entityId[map$plotRef == 'zAxisVariable'],
                          'dataType' = map$dataType[map$plotRef == 'zAxisVariable'])
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
 
  .heatmap <- newHeatmapPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            zAxisVariable = zAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value)

  .heatmap <- validateHeatmapPD(.heatmap)

  return(.heatmap)

}

#' Heatmap data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap.
#' 1) 'collection' of numeric variables vs single categorical
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @return character name of json file containing plot-ready data
#' @export
heatmap <- function(data, map, value = c('series','collection')) {
  value <- match.arg(value)
  .heatmap <- heatmap.dt(data, map, value)
  outFileName <- writeJSON(.heatmap, 'heatmap')

  return(outFileName)
}

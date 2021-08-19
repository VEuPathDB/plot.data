newHeatmapPD <- function(.dt = data.table::data.table(),
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
                         zAxisVariable = list('variableId' = NULL,
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
                         value = character(),
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
                     class = "heatmap")

  attr <- attributes(.pd)
  attr$yAxisVariable <- yAxisVariable
  attr$zAxisVariable <- zAxisVariable

  #NOTE: one or the other of these could be a list for 'collection'
  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  #NOTE: this for the case of 'series'
  z <- toColNameOrNull(attr$zAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

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

  setAttrFromList(.pd, attr)
  
  return(.pd)
}

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
#' There are two ways to calculate z-values for the heatmap. \cr
#' 1) 'collection' of numeric variables vs single categorical \cr
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @return data.table plot-ready data
#' @export
heatmap.dt <- function(data, map, 
                       value = c('series', 'collection'), 
                       evilMode = c(FALSE, TRUE),
                       verbose = c(TRUE, FALSE)) {

  value <- matchArg(value)
  evilMode <- matchArg(evilMode)
  verbose <- matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type scatter.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    stop("Must provide yAxisVariable for plot type scatter.")
  }
  zAxisVariable <- plotRefMapToList(map, 'zAxisVariable')
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
 
  .heatmap <- newHeatmapPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            zAxisVariable = zAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value = value,
                            evilMode = evilMode,
                            verbose = verbose)

  .heatmap <- validateHeatmapPD(.heatmap, verbose)

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
#' There are two ways to calculate z-values for the heatmap. \cr
#' 1) 'collection' of numeric variables vs single categorical \cr
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @return character name of json file containing plot-ready data
#' @export
heatmap <- function(data, map, 
                    value = c('series','collection'), 
                    evilMode = c(FALSE, TRUE)) {
  verbose <- matchArg(verbose)
 
  .heatmap <- heatmap.dt(data, map, value, evilMode, verbose)
  logWithTime('New heatmap object created!', verbose)
  outFileName <- writeJSON(.heatmap, evilMode, 'heatmap', verbose)

  return(outFileName)
}

newBarPD <- function(.dt = data.table::data.table(),
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
                     class = "barplot")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  group <- veupathUtils::toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)
  .pd[[x]] <- as.character(.pd[[x]])

  if (value == 'identity') {
    .pd <- collapseByGroup(.pd, group, panel)
    veupathUtils::logWithTime('Value is set to `identity`. Resulting barplot object will represent raw values.', verbose)
  } else if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, x, 'dummy', group, panel, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))
    veupathUtils::logWithTime('Value is set to `count`. Resulting barplot object will represent counts of unique x-axis values per group.', verbose)
  } else if (value == 'proportion') {
    .pd$dummy <- 1
    .pd <- groupProportion(.pd, x, 'dummy', group, panel, barmode, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))
    veupathUtils::logWithTime('Value is set to `proportion`. If barmode is `group` the resulting barplot object will represent the relative proportions of unique x-axis values across groups. If barmode is `stack` the resulting barplot object will represent the proportions of unique x-axis values relative to the total x-axis values in that panel.', verbose)
  }
  
  attr$names <- names(.pd)
  
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateBarPD <- function(.bar, verbose) {
  veupathUtils::logWithTime('Barplot request has been validated!', verbose)

  return(.bar)
}

#' Bar Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are three options to calculate y-values for plotting. \cr
#' 1) raw 'identity' of values from data.table input \cr
#' 2) 'count' occurrences of values from data.table input \cr 
#' 3) 'proportion' of occurrences of values from data.table input \cr 
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' - naToZero: Options are TRUE, FALSE, or ''. Optional. Indicates whether to replaces NAs with 0, assuming the column is numeric. If set to TRUE, all NAs found within the column should be replaced with 0. Passing '' will result in using the function default, which in plot.data is FALSE. Setting naToZero=TRUE for a string var will throw an error. \cr
#' @return data.table plot-ready data
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. See section below for organization.
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- bar.dt(df,map,value='count')
#' @export
bar.dt <- function(data, 
                   map, 
                   value = c('count', 'identity', 'proportion'), 
                   barmode = c('group', 'stack'), 
                   evilMode = c(FALSE, TRUE),
                   verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  barmode <- veupathUtils::matchArg(barmode)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type bar.")
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .bar <- newBarPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    value = value,
                    barmode = barmode,
                    evilMode = evilMode,
                    verbose = verbose)

  .bar <- validateBarPD(.bar, verbose)
  veupathUtils::logWithTime(paste('New barplot object created with parameters value =', value, ', barmode =', barmode, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.bar)
}

#' Bar Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are three options to calculate y-values for plotting. \cr
#' 1) raw 'identity' of values from data.table input \cr
#' 2) 'count' occurrences of values from data.table input \cr 
#' 3) 'proportion' of occurrences of values from data.table input \cr
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' - naToZero: Options are TRUE, FALSE, or ''. Optional. Indicates whether to replaces NAs with 0, assuming the column is numeric. If set to TRUE, all NAs found within the column should be replaced with 0. Passing '' will result in using the function default, which in plot.data is FALSE. Setting naToZero=TRUE for a string var will throw an error. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot.
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#'
#' # Returns the name of a json file
#' bar(df,map,value='count')
#' @return character name of json file containing plot-ready data
#' @export
bar <- function(data, 
                map, 
                value = c('count', 'identity', 'proportion'), 
                barmode = c('group', 'stack'), 
                evilMode = c(FALSE, TRUE),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .bar <- bar.dt(data, map, value, barmode, evilMode, verbose)
  outFileName <- writeJSON(.bar, evilMode, 'barplot', verbose)

  return(outFileName)
}

newPiePD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
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
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "pieplot")

  attr <- attributes(.pd)


  # TODO here well have to check if xAxis has >8 values and rank if so. 
  # also make an attr listing the ranked values.

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)
  .pd[[x]] <- as.character(.pd[[x]])

  if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, x, 'dummy', NULL, panel, collapse = T)  
    veupathUtils::logWithTime('Value is set to `count`. Resulting pieplot object will represent counts of unique x-axis values per panel.', verbose)
  } else if (value == 'proportion') {
    .pd$dummy <- 1
    .pd <- groupProportion(.pd, x, 'dummy', NULL, panel, 'group', collapse = T)
    veupathUtils::logWithTime('Value is set to `proportion`. Resulting pieplot object will represent the relative proportions of unique xAxis values across panels.', verbose)
  }
  data.table::setnames(.pd, c(panel, 'label', 'value'))
  attr$names <- names(.pd)
  
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validatePiePD <- function(.pie, verbose) {
  veupathUtils::logWithTime('Pieplot request has been validated!', verbose)

  return(.pie)
}

#' Pie Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'panel' specifies the facet panel the series data belongs to.
#' There are two options to calculate y-values for plotting. \cr
#' 1) 'count' occurrences of values from data.table input \cr 
#' 2) 'proportion' of occurrences of values from data.table input \cr 
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
#' @return data.table plot-ready data
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. See section below for organization.
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xAxis' = sample(c('a','b','c'), 100, replace=T),
#'                  'facet' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('facet', 'xAxis'),
#'                  'plotRef' = c('facetVariable', 'xAxisVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- pie.dt(df,map,value='count')
#' @export
pie.dt <- function(data, 
                   map, 
                   value = c('count', 'proportion'),  
                   evilMode = c(FALSE, TRUE),
                   verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type pie.")
  }
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .pie <- newPiePD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    value = value,
                    evilMode = evilMode,
                    verbose = verbose)

  .pie <- validatePiePD(.pie, verbose)
  veupathUtils::logWithTime(paste('New pieplot object created with parameters value =', value, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.pie)
}

#' Pie Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'panel' specifies the facet panel the series data belongs to.
#' There are two options to calculate y-values for plotting. \cr
#' 1) 'count' occurrences of values from data.table input \cr 
#' 2) 'proportion' of occurrences of values from data.table input \cr
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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot.
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xAxis' = sample(c('a','b','c'), 100, replace=T),
#'                  'facet' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('facet', 'xAxis'),
#'                  'plotRef' = c('facetVariable', 'xAxisVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#'
#' # Returns the name of a json file
#' pie(df,map,value='count')
#' @return character name of json file containing plot-ready data
#' @export
pie <- function(data, 
                map, 
                value = c('count', 'proportion'), 
                evilMode = c(FALSE, TRUE),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .pie <- pie.dt(data, map, value, evilMode, verbose)
  outFileName <- writeJSON(.pie, evilMode, 'pieplot', verbose)

  return(outFileName)
}

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

  x <- toColNameOrNull(attr$xAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
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
#  xAxisVariable <- attr(.bar, 'xAxisVariable')
#  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#    stop('The independent axis must be binary, ordinal or categorical for barplot.')
#  }
#  overlayVariable <- attr(.bar, 'overlayVariable')
#  if (!is.null(overlayVariable)) {
#    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The overlay variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable1 <- attr(.bar, 'facetVariable1')
#  if (!is.null(facetVariable1)) {
#    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The first facet variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable2 <- attr(.bar, 'facetVariable2')
#  if (!is.null(facetVariable2)) {
#    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The second facet variable must be binary, ordinal or categorical.')
#    }
#  }
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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
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
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
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

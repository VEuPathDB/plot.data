newLinePD <- function(.dt = data.table::data.table(),
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
                         listVarDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'listVarPlotRef' = NULL,
                                               'listVarDisplayLabel' = NULL),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     listVarDetails = listVarDetails,
                     verbose = verbose,
                     class = "lineplot")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  y <- veupathUtils::toColNameOrNull(attr$yAxisVariable)
  group <- veupathUtils::toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  if (value == 'mean') {
    
    mean <- groupMean(.pd, x, y, group, panel)
    data.table::setnames(mean, c(group, panel, 'seriesX', 'seriesY'))
    .pd <- mean
    veupathUtils::logWithTime('Mean calculated per X-axis value.', verbose)

  } else if (value == 'median') {

     median <- groupMedian(.pd, x, y, group, panel)
     data.table::setnames(median, c(group, panel, 'seriesX', 'seriesY'))
    .pd <- median
    veupathUtils::logWithTime('Median calculated per X-axis value.', verbose)

  }
  if (attr$xAxisVariable$dataType == 'DATE') {
    .pd$seriesX <- lapply(.pd$seriesX, format, '%Y-%m-%d')
  } else { 
    .pd$seriesX <- lapply(.pd$seriesX, as.character)
  }
  if (attr$yAxisVariable$dataType == 'DATE') {
    .pd$seriesY <- lapply(.pd$seriesY, format, '%Y-%m-%d')
  } else {
    .pd$seriesY <- lapply(.pd$seriesY, as.character)
  }
  attr$names <- names(.pd)

  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateLinePD <- function(.line, verbose) {
#  xAxisVariable <- attr(.line, 'xAxisVariable')
#  if (!xAxisVariable$dataShape %in% c('CONTINUOUS','ORDINAL')) {
#    stop('The independent axis must be continuous or ordinal for lineplot.')
#  }
#  yAxisVariable <- attr(.line, 'yAxisVariable')
#  if (!yAxisVariable$dataShape %in% c('CONTINUOUS')) {
#    stop('The dependent axis must be continuous for lineplot.')
#  }
#  overlayVariable <- attr(.line, 'overlayVariable')
#  if (!is.null(overlayVariable)) {
#    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL', 'CONTINUOUS')) {
#      stop('The overlay variable must be binary, ordinal, categorical, or continuous.')
#    }
#  }
#  facetVariable1 <- attr(.line, 'facetVariable1')
#  if (!is.null(facetVariable1)) {
#    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The first facet variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable2 <- attr(.line, 'facetVariable2')
#  if (!is.null(facetVariable2)) {
#    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The second facet variable must be binary, ordinal or categorical.')
#    }
#  }
  veupathUtils::logWithTime('Line plot request has been validated!', verbose)

  return(.line)
}

#' Line Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' line plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases 
#' for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data 
#' for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable 
#' sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 
#' 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'mean', 'median' for y-axis
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of 
#' variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to 
#' the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned 
#' to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(1:20, 100, replace=T),
#'                  'yvar' = rnorm(100), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER'),
#'                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- lineplot.dt(df, map, value = 'median')
#' @export
lineplot.dt <- function(data, 
                         map, 
                         value = c('mean',
                                   'median'),
                         evilMode = c(FALSE, TRUE),
                         listVarPlotRef = NULL,
                         listVarDisplayLabel = NULL,
                         inferredVarDisplayLabel = NULL,
                         verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode) 
  verbose <- veupathUtils::matchArg(verbose)  

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  map <- validateMap(map)
  veupathUtils::logWithTime('Map has been validated.', verbose)

  # If there is a duplicated plotRef in map, it must match listVarPlotRef
  if (any(duplicated(map$plotRef))) {
    if (!identical(listVarPlotRef, unique(map$plotRef[duplicated(map$plotRef)]))) {
      stop('listVar error: duplicated map plotRef does not match listVarPlotRef.')
    }
  }

  # If listVar and inferredVar labels are provided, must also provide listVarPlotRef
  if ((!is.null(listVarDisplayLabel) | !is.null(inferredVarDisplayLabel)) & is.null(listVarPlotRef)) {
    stop('listVar error: listVarPlotRef must be specified in order to use inferredVarDisplayLabel or listVarDisplayLabel')
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type line.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    if (is.null(listVarPlotRef)) {
      stop("Must provide yAxisVariable for plot type line.")
    }
  } 
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  # Handle listVars
  listVarDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'listVarPlotRef' = listVarPlotRef,
                         'listVarDisplayLabel' = listVarDisplayLabel)
  if (!is.null(listVarPlotRef)) {
    if (identical(listVarPlotRef, 'overlayVariable')) { 
      inferredVarEntityId <- unique(overlayVariable$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable1')) { 
      inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable2')) { 
      inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { 
      stop('listVar error: listVarPlotRef must be either overlayVariable, facetVariable1, or facetVariable2 for line.')
    }

    listVarDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS',
                                          'displayLabel' = inferredVarDisplayLabel)

    veupathUtils::logWithTime('Created inferred variable from listVariable.', verbose)
  }

  .line <- newLinePD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value = value,
                            evilMode = evilMode,
                            listVarDetails = listVarDetails,
                            verbose = verbose)

  .line <- validateLinePD(.line, verbose)
  veupathUtils::logWithTime(paste('New line plot object created with parameters value =', value, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.line)
}

#' Line Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' line plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete 
#' cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for 
#' the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId 
#' and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 
#' 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'mean', 'median' for y-axis
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of variables 
#' should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to 
#' the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned 
#' to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(1:20, 100, replace=T),
#'                  'yvar' = rnorm(100), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER'),
#'                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
#' 
#' # Returns the name of a json file
#' lineplot(df, map, value = 'median')
#' @export
lineplot <- function(data,
                      map,
                      value = c('mean', 
                                'median'),
                      evilMode = c(FALSE, TRUE),
                      listVarPlotRef = NULL,
                      listVarDisplayLabel = NULL,
                      inferredVarDisplayLabel = NULL,
                      verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .line <- lineplot.dt(data,
                           map,
                           value = value,
                           evilMode = evilMode,
                           listVarPlotRef = listVarPlotRef,
                           listVarDisplayLabel = listVarDisplayLabel,
                           inferredVarDisplayLabel = inferredVarDisplayLabel,
                           verbose = verbose)
                           
  outFileName <- writeJSON(.line, evilMode, 'lineplot', verbose)

  return(outFileName)
}

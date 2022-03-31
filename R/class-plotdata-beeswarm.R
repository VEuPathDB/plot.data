newBeeswarmPD <- function(.dt = data.table::data.table(),
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
                         jitter = NULL,
                         median = logical(),
                         evilMode = character(),
                         collectionVariableDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'collectionVariablePlotRef' = NULL),
                         computedVariableMetadata = list('displayName' = NULL,
                                                         'displayRangeMin' = NULL,
                                                         'displayRangeMax' = NULL,
                                                         'collectionVariable' = NULL),
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
                     collectionVariableDetails = collectionVariableDetails,
                     computedVariableMetadata = computedVariableMetadata,
                     verbose = verbose,
                     class = "beeswarm")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  y <- veupathUtils::toColNameOrNull(attr$yAxisVariable)
  group <- veupathUtils::toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)


  # Organize raw data and compute jittered values. Keeping all jittered values centered at 0 so that the front end can reorder if necessary.
  byCols <- colnames(.pd)[colnames(.pd) %in% c(x, group, panel)]
  rawWithJitter <- .pd[, list(rawData=lapply(.SD, as.vector),
                        jitteredValues=lapply(.SD, function(x, jitter) runif(length(x), min=(-jitter), max=jitter), jitter=jitter)), keyby=byCols]
  
  byColValues <- unique(.pd[, byCols, with=FALSE])
  rawWithJitter <- merge(rawWithJitter, byColValues, by=byCols, all=TRUE)

  rawWithJitter <- collapseByGroup(rawWithJitter, group, panel)
  # indexCols <- c(panel, group)
  # setkeyv(rawWithJitter, indexCols)
  
  .pd.base <- rawWithJitter
  logWithTime('Returning all points for beeswarm.', verbose)

  if (median) {
    median <- groupMedian(.pd, x, y, group, panel)
    median[[x]] <- NULL
    if (!is.null(key(median))) {
      .pd.base <- merge(.pd.base, median)
    } else {
      .pd.base <- cbind(.pd.base, median)
    }
    logWithTime('Calculated medians for beeswarm.', verbose)
  }
  
  .pd <- .pd.base
  data.table::setnames(.pd, x, 'label')
  attr$names <- names(.pd)
  setAttrFromList(.pd, attr)

  return(.pd)
}

validateBeeswarmPD <- function(.beeswarm, verbose) {
  yAxisVariable <- attr(.beeswarm, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER', 'INTEGER')) {
    stop('The dependent axis must be of type number for beeswarm.')
  }
  logWithTime('Beeswarm request has been validated!', verbose)

  return(.beeswarm)
}


#' Beeswarm Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'jitteredValues' represent the x axis tick label and a random offset (one per y value), repsectively.
#' The 'rawData' column lists the y values to be plotted above each x axis tick. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return median values per group
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
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
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- beeswarm.dt(df, map, jitter=0.3)
#' @export
beeswarm.dt <- function(data, map,
                   jitter = NULL, 
                   median = c(FALSE, TRUE), 
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   collectionVariablePlotRef = NULL,
                   computedVariableMetadata = NULL,
                   verbose = c(TRUE, FALSE)) {

  median <- matchArg(median)
  evilMode <- matchArg(evilMode)
  verbose <- matchArg(verbose)

  # Set default jitter to 0.1 (should also test is numeric)
  if (is.null(jitter)) {
    jitter <- 0.1
  } else if (!is.numeric(jitter)) {
    stop('jitter must be numeric for beeswarm plots.')
  }

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  map <- validateMap(map)
  logWithTime('Map has been validated.', verbose)

  # If there is a duplicated plotRef in map, it must match collectionVariablePlotRef
  if (any(duplicated(map$plotRef))) {
    if (!identical(collectionVariablePlotRef, unique(map$plotRef[duplicated(map$plotRef)]))) {
      stop('collectionVar error: duplicated map plotRef does not match collectionVariablePlotRef.')
    }
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type beeswarm.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId) & is.null(collectionVariablePlotRef)) {
    stop("Must provide yAxisVariable for plot type beeswarm.")
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  
  # Handle collectionVars
  collectionVariableDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'collectionVariablePlotRef' = collectionVariablePlotRef)

  if (!is.null(collectionVariablePlotRef)) {
    if (identical(collectionVariablePlotRef, 'xAxisVariable')) { inferredVarEntityId <- unique(xAxisVariable$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable1')) { inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable2')) { inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { stop('collectionVar error: collectionVariablePlotRef must be either xAxisVariable, facetVariable1, or facetVariable2 for beeswarm.')
    }

    collectionVariableDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS')

    logWithTime('Created inferred variable from collectionVariable.', verbose)
  }

  .beeswarm <- newBeeswarmPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    jitter = jitter,
                    median = median,
                    evilMode = evilMode,
                    collectionVariableDetails = collectionVariableDetails,
                    computedVariableMetadata = computedVariableMetadata,
                    verbose = verbose)

  .beeswarm <- validateBeeswarmPD(.beeswarm, verbose)
  logWithTime(paste('New beeswarm object created with parameters jitter=', jitter, ', median =', median, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.beeswarm) 

}

#' Beeswarm Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'jitteredValues' represent the x axis tick label and a random offset (one per y value), repsectively.
#' The 'rawData' column lists the y values to be plotted above each x axis tick. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
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
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables')
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'yvar' = rnorm(100),
#'                  'overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar', 'overlay'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
#'                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns the name of a json file
#' beeswarm(df,map,jitter=0.3)
#' @export
beeswarm <- function(data, map, 
                jitter = NULL, 
                median = c(FALSE, TRUE), 
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                collectionVariablePlotRef = NULL,
                computedVariableMetadata = NULL,
                verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .beeswarm <- beeswarm.dt(data,
                 map,
                 jitter = jitter,
                 median = median,
                 evilMode = evilMode,
                 collectionVariablePlotRef = collectionVariablePlotRef,
                 computedVariableMetadata = computedVariableMetadata,
                 verbose = verbose)
  outFileName <- writeJSON(.beeswarm, evilMode, 'beeswarm', verbose)

  return(outFileName)
}

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
                     class = "beeswarm")

  attr <- attributes(.pd)
  print(jitter)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)


  # Organize raw data and compute jittered values
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
  xAxisVariable <- attr(.beeswarm, 'xAxisVariable')
  #if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #  stop('The independent axis must be binary, ordinal or categorical for beeswarm.')
  #}
  yAxisVariable <- attr(.beeswarm, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number for beeswarm.')
  }
  overlayVariable <- attr(.beeswarm, 'overlayVariable')
  #if (!is.null(overlayVariable)) {
  #  if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The overlay variable must be binary, ordinal or categorical.')
  #  }
  #}
  facetVariable1 <- attr(.beeswarm, 'facetVariable1')
  #if (!is.null(facetVariable1)) {
  #  if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The first facet variable must be binary, ordinal or categorical.')
  #  }
  #}
  facetVariable2 <- attr(.beeswarm, 'facetVariable2')
  #if (!is.null(facetVariable2)) {
  #  if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
  #    stop('The second facet variable must be binary, ordinal or categorical.')
  #  }
  #}
  logWithTime('Beeswarm request has been validated!', verbose)

  return(.beeswarm)
}


#' Beeswarm Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @export

beeswarm.dt <- function(data, map,
                   jitter = NULL, 
                   median = c(FALSE, TRUE), 
                   evilMode = c(FALSE, TRUE),
                   listVarPlotRef = NULL,
                   listVarDisplayLabel = NULL,
                   inferredVarDisplayLabel = NULL,
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
    stop("Must provide xAxisVariable for plot type beeswarm.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId) & is.null(listVarPlotRef)) {
    stop("Must provide yAxisVariable for plot type beeswarm.")
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
    if (identical(listVarPlotRef, 'xAxisVariable')) { inferredVarEntityId <- unique(xAxisVariable$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable1')) { inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable2')) { inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { stop('listVar error: listVarPlotRef must be either xAxisVariable, facetVariable1, or facetVariable2 for beeswarm.')
    }

    listVarDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS',
                                          'displayLabel' = inferredVarDisplayLabel)

    logWithTime('Created inferred variable from listVariable.', verbose)
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
                    listVarDetails = listVarDetails,
                    verbose = verbose)

  .beeswarm <- validateBeeswarmPD(.beeswarm, verbose)
  logWithTime(paste('New beeswarm object created with parameters jitter=', jitter, ', median =', median, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.beeswarm) 

}

#' Beeswarm Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'xAxisVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @export
beeswarm <- function(data, map, 
                jitter = NULL, 
                median = c(FALSE, TRUE), 
                evilMode = c(FALSE, TRUE),
                listVarPlotRef = NULL,
                listVarDisplayLabel = NULL,
                inferredVarDisplayLabel = NULL,
                verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .beeswarm <- beeswarm.dt(data,
                 map,
                 jitter = jitter,
                 median = median,
                 evilMode = evilMode,
                 listVarPlotRef = listVarPlotRef,
                 listVarDisplayLabel = listVarDisplayLabel,
                 inferredVarDisplayLabel = inferredVarDisplayLabel,
                 verbose = verbose)
  outFileName <- writeJSON(.beeswarm, evilMode, 'beeswarm', verbose)

  return(outFileName)
}

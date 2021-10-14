newScatterPD <- function(.dt = data.table::data.table(),
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
                     class = "scatterplot")

  attr <- attributes(.pd)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  #if (identical(attr$overlayVariable$dataShape,'CONTINUOUS')) {
  #  series <- collapseByGroup(.pd, group = NULL, panel)
  #  data.table::setnames(series, c(panel, 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  #} else {
    series <- collapseByGroup(.pd, group, panel)
    data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
  #}
 
  if (attr$xAxisVariable$dataType == 'DATE') {
    series$seriesX <- lapply(series$seriesX, format, '%Y-%m-%d')
  } else { 
    series$seriesX <- lapply(series$seriesX, as.character)
  }
  if (attr$yAxisVariable$dataType == 'DATE') {
    series$seriesY <- lapply(series$seriesY, format, '%Y-%m-%d')
  } else {
    series$seriesY <- lapply(series$seriesY, as.character)
  }
  logWithTime('Collected raw scatter plot data.', verbose)

  if (value == 'smoothedMean') {

    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    .pd <- smoothedMean
    logWithTime('Calculated smoothed means.', verbose)

  } else if (value == 'smoothedMeanWithRaw') {
    
    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, smoothedMean)
    } else {
      .pd <- cbind(series, smoothedMean)
    }
    logWithTime('Calculated smoothed means.', verbose)

  } else if (value == 'bestFitLineWithRaw') {
  
    bestFitLine <- groupBestFitLine(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, bestFitLine)
    } else {
      .pd <- cbind(series, bestFitLine)
    }
    logWithTime('Calculated best fit line.', verbose)

  } else if (value == 'density') {
    
    density <- groupDensity(.pd, NULL, x, group, panel)
    .pd <- density
    logWithTime('Kernel density estimated calculated from raw data.', verbose)

  } else {
    .pd <- series
  }
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

validateScatterPD <- function(.scatter, verbose) {
#  xAxisVariable <- attr(.scatter, 'xAxisVariable')
#  if (!xAxisVariable$dataShape %in% c('CONTINUOUS','ORDINAL')) {
#    stop('The independent axis must be continuous or ordinal for scatterplot.')
#  }
#  yAxisVariable <- attr(.scatter, 'yAxisVariable')
#  if (!yAxisVariable$dataShape %in% c('CONTINUOUS')) {
#    stop('The dependent axis must be continuous for scatterplot.')
#  }
#  overlayVariable <- attr(.scatter, 'overlayVariable')
#  if (!is.null(overlayVariable)) {
#    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL', 'CONTINUOUS')) {
#      stop('The overlay variable must be binary, ordinal, categorical, or continuous.')
#    }
#  }
#  facetVariable1 <- attr(.scatter, 'facetVariable1')
#  if (!is.null(facetVariable1)) {
#    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The first facet variable must be binary, ordinal or categorical.')
#    }
#  }
#  facetVariable2 <- attr(.scatter, 'facetVariable2')
#  if (!is.null(facetVariable2)) {
#    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
#      stop('The second facet variable must be binary, ordinal or categorical.')
#    }
#  }
  logWithTime('Scatter plot request has been validated!', verbose)

  return(.scatter)
}

#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'smoothedMeanX', 
#' 'smoothedMeanY' and 'smoothedMeanSE' specify the x, y and 
#' standard error respectively of the smoothed conditional mean 
#' for the group. Columns 'densityX' and 'densityY' contain the 
#' calculated kernel density estimates. Column 
#' 'seriesGradientColorscale' contains values to be used with a 
#' gradient colorscale when plotting.
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
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, 
                         map, 
                         value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw'),
                         evilMode = c(FALSE, TRUE),
                         listVarPlotRef = NULL,
                         listVarDisplayLabel = NULL,
                         inferredVarDisplayLabel = NULL,
                         verbose = c(TRUE, FALSE)) {

  value <- matchArg(value)
  evilMode <- matchArg(evilMode) 
  verbose <- matchArg(verbose)  

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
    stop("Must provide xAxisVariable for plot type scatter.")
  } else {
    if (!xAxisVariable$dataType %in% c('NUMBER','INTEGER') & value == 'density') {
      stop('Density curves can only be provided for numeric independent axes.')
    }
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    if (is.null(listVarPlotRef)) {
      stop("Must provide xAxisVariable for plot type scatter.")
    }
  } else {
    if (!yAxisVariable$dataType %in% c('NUMBER', 'INTEGER') & value != 'raw') {
      stop('Trend lines can only be provided for numeric dependent axes.')
    }
  } 
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  if (!is.null(overlayVariable$variableId) & !identical(listVarPlotRef, 'overlayVariable')) {
    #if (overlayVariable$dataShape == 'CONTINUOUS' & value != 'raw') {
    #  stop('Continuous overlay variables cannot be used with trend lines.')
    #}
  }
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  # Handle listVars
  listVarDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'listVarPlotRef' = listVarPlotRef,
                         'listVarDisplayLabel' = listVarDisplayLabel)
  if (!is.null(listVarPlotRef)) {
    if (identical(listVarPlotRef, 'overlayVariable')) { inferredVarEntityId <- unique(overlayVariable$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable1')) { inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(listVarPlotRef, 'facetVariable2')) { inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { stop('listVar error: listVarPlotRef must be either overlayVariable, facetVariable1, or facetVariable2 for scatter.')
    }

    listVarDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS',
                                          'displayLabel' = inferredVarDisplayLabel)

    logWithTime('Created inferred variable from listVariable.', verbose)
  }

  .scatter <- newScatterPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value = value,
                            evilMode = evilMode,
                            listVarDetails = listVarDetails,
                            verbose = verbose)

  .scatter <- validateScatterPD(.scatter, verbose)
  logWithTime(paste('New scatter plot object created with parameters value =', value, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.scatter)
}

#' Scatter Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'smoothedMeanX', 
#' 'smoothedMeanY' and 'smoothedMeanSE' specify the x, y and 
#' standard error respectively of the smoothed conditional mean 
#' for the group. Columns 'densityX' and 'densityY' contain the 
#' calculated kernel density estimates. Column 
#' 'seriesGradientColorscale' contains values to be used with a 
#' gradient colorscale when plotting.
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
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param listVarPlotRef string indicating the plotRef to be considered as a listVariable. Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of variables should be interpreted as a listVariable.
#' @param listVarDisplayLabel string indicating the final displayLabel to be assigned to the repeated variable.
#' @param inferredVarDisplayLabel string indicated the final displayLabel to be assigned to the inferred variable.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data,
                      map,
                      value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw'),
                      evilMode = c(FALSE, TRUE),
                      listVarPlotRef = NULL,
                      listVarDisplayLabel = NULL,
                      inferredVarDisplayLabel = NULL,
                      verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .scatter <- scattergl.dt(data,
                           map,
                           value = value,
                           evilMode = evilMode,
                           listVarPlotRef = listVarPlotRef,
                           listVarDisplayLabel = listVarDisplayLabel,
                           inferredVarDisplayLabel = inferredVarDisplayLabel,
                           verbose = verbose)
                           
  outFileName <- writeJSON(.scatter, evilMode, 'scattergl', verbose)

  return(outFileName)
}

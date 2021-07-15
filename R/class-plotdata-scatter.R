newScatterPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         value = character(),
                         evilMode = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     class = "scatterplot")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  if (identical(attr$overlayVariable$dataShape,'CONTINUOUS')) {
    series <- collapseByGroup(.pd, group = NULL, panel)
    data.table::setnames(series, c(panel, 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  } else {
    series <- collapseByGroup(.pd, group, panel)
    data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
  }
  
  series$seriesX <- lapply(series$seriesX, as.character)
  series$seriesY <- lapply(series$seriesY, as.character)

  if (value == 'smoothedMean') {

    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    .pd <- smoothedMean

  } else if (value == 'smoothedMeanWithRaw') {
    
    smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, smoothedMean)
    } else {
      .pd <- cbind(series, smoothedMean)
    }

  } else if (value == 'bestFitLineWithRaw') {
  
    bestFitLine <- groupBestFitLine(.pd, x, y, group, panel)
    if (!is.null(key(series))) {
      .pd <- merge(series, bestFitLine)
    } else {
      .pd <- cbind(series, bestFitLine)
    }

  } else if (value == 'density') {
    
    density <- groupDensity(.pd, x, group, panel)
    .pd <- density
    
  } else {
    .pd <- series
  }
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

validateScatterPD <- function(.scatter) {
  xAxisVariable <- attr(.scatter, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('CONTINUOUS','ORDINAL')) {
    stop('The independent axis must be continuous or ordinal for scatterplot.')
  }
  yAxisVariable <- attr(.scatter, 'yAxisVariable')
  if (!yAxisVariable$dataShape %in% c('CONTINUOUS')) {
    stop('The dependent axis must be continuous for scatterplot.')
  }
  overlayVariable <- attr(.scatter, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL', 'CONTINUOUS')) {
      stop('The overlay variable must be binary, ordinal, categorical, or continuous.')
    }
  }
  facetVariable1 <- attr(.scatter, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.scatter, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }

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
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, 
                         map, 
                         value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw'),
                         evilMode = c(FALSE, TRUE)) {

  value <- matchArg(value)
  evilMode <- matchArg(evilMode) 
  
  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL,
                         'dataShape' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL,
                        'dataShape' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL,
                        'dataShape' = NULL)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }
  
  
  # Handle repeated plot references
  if (any(duplicated(map$plotRef))) {

    # Identify the list var based on any plotRef that is repeated
    listVarPlotRef <- unique(map$plotRef[duplicated(map$plotRef)])
    listVarPlotRef <- validateListVar(map, listVarPlotRef)
    
    # Scatter-specific
    if (listVarPlotRef == 'facetVariable1' | listVarPlotRef == 'overlayVariable') {
      meltedValuePlotRef <- 'yAxisVariable'
    } else {
      stop("Incompatable repeated variable")
    }
    
    # Check to ensure meltedValuePlotRef is not already defined
    if (any(map$plotRef == meltedValuePlotRef)) {
      stop(paste0("Cannot melt data: ", meltedValuePlotRef, " already defined."))
    }
    
    # Record variable order
    listVarIdOrder <- map$id[map$plotRef == listVarPlotRef]
    
    # Melt data and update the map 
    data <- data.table::melt(data, measure.vars = listVarIdOrder, variable.factor = FALSE, variable.name='meltedVariable', value.name='meltedValue')
    map <- remapListVar(map, listVarPlotRef, meltedValuePlotRef)
    
  } # end handling of repeated plot element references

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
    if (xAxisVariable$dataType != 'NUMBER' & value == 'density') {
      stop('Density curves can only be provided for numeric independent axes.')
    }
  } else {
    stop("Must provide xAxisVariable for plot type scatter.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
    if (yAxisVariable$dataType != 'NUMBER' & value != 'raw') {
      stop('Trend lines can only be provided for numeric dependent axes.')
    }
  } else {
    stop("Must provide yAxisVariable for plot type scatter.")
  }
  if ('overlayVariable' %in% map$plotRef) {
    overlayVariable <- plotRefMapToList(map, 'overlayVariable')
    if (overlayVariable$dataShape == 'CONTINUOUS' & value != 'raw') {
      stop('Continuous overlay variables cannot be used with trend lines.')
    }
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }

  .scatter <- newScatterPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            value = value,
                            evilMode = evilMode)

  .scatter <- validateScatterPD(.scatter)

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
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw'), evilMode = c(FALSE, TRUE)) {

  value <- matchArg(value)
  evilMode <- matchArg(evilMode)

  .scatter <- scattergl.dt(data, map, value, evilMode)
  outFileName <- writeJSON(.scatter, evilMode, 'scattergl')

  return(outFileName)
}

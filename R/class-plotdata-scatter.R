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
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "scatterplot")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  series <- collapseByGroup(.pd, group, panel)
  data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
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
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The overlay variable must be binary, ordinal or categorical.')
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
#' calculated kernel density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, 
                         map, 
                         value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw')) {
  value <- match.arg(value)

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
    repeatedPlotRef <- extractListVar(map)
    
    # Scatter-specific flows
    #### Currently left un-optimized to ensure we have correct flows. 
    if (repeatedPlotRef == 'facetVariable1') {
      meltedVarPlotRef <- 'facetVariable1'
      meltedValuePlotRef <- 'yAxisVariable'
    } else {
      stop("Incompatable repeated variable")
    }
    
    # Check to ensure meltedValuePlotRef is not already defined
    if (any(map$plotRef == meltedValuePlotRef)) {
      stop(paste0("Cannot melt data: ", meltedValuePlotRef, " already defined."))
    }
    
    # Record variable order
    repeatedVarIdOrder <- map$id[map$plotRef == repeatedPlotRef]
    
    # Melt data and update the map 
    data <- data.table::melt(data, measure.vars = repeatedVarIdOrder, variable.factor = FALSE, variable.name='meltedVariable', value.name='meltedValue')
    map <- remapVariableList(map, repeatedPlotRef, meltedVarPlotRef, meltedValuePlotRef)
    
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
                            value)

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
#' calculated kernel density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, value = c('smoothedMean', 'smoothedMeanWithRaw', 'bestFitLineWithRaw', 'density', 'raw')) {
  value <- match.arg(value)
  .scatter <- scattergl.dt(data, map, value)
  outFileName <- writeJSON(.scatter, 'scattergl')

  return(outFileName)
}

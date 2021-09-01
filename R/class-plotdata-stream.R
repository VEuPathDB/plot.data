newStreamPD <- function(.dt = data.table::data.table(),
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
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = F,
                     verbose = verbose,
                     class = "stream")

  attr <- attributes(.pd)

  x <- toColNameOrNull(attr$xAxisVariable)
  y <- toColNameOrNull(attr$yAxisVariable)
  group <- toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)
  
  # For stacked area, order by x
  data.table::setorderv(.pd, x)

  if (identical(attr$overlayVariable$dataShape,'CONTINUOUS')) {
    series <- collapseByGroup(.pd, group = NULL, panel)
    data.table::setnames(series, c(panel, 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  } else {
    series <- collapseByGroup(.pd, group, panel)
    data.table::setnames(series, c(group, panel, 'seriesX', 'seriesY'))
  }
 
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
  logWithTime('Collected raw stream plot data.', verbose)


#### Do stacked area things


  # if (value == 'smoothedMean') {

  #   smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
  #   .pd <- smoothedMean
  #   logWithTime('Calculated smoothed means.', verbose)

  # } else if (value == 'smoothedMeanWithRaw') {
    
  #   smoothedMean <- groupSmoothedMean(.pd, x, y, group, panel)
  #   if (!is.null(key(series))) {
  #     .pd <- merge(series, smoothedMean)
  #   } else {
  #     .pd <- cbind(series, smoothedMean)
  #   }
  #   logWithTime('Calculated smoothed means.', verbose)

  # } else if (value == 'bestFitLineWithRaw') {
  
  #   bestFitLine <- groupBestFitLine(.pd, x, y, group, panel)
  #   if (!is.null(key(series))) {
  #     .pd <- merge(series, bestFitLine)
  #   } else {
  #     .pd <- cbind(series, bestFitLine)
  #   }
  #   logWithTime('Calculated best fit line.', verbose)

  # } else if (value == 'density') {
    
  #   density <- groupDensity(.pd, NULL, x, group, panel)
  #   .pd <- density
  #   logWithTime('Kernel density estimated calculated from raw data.', verbose)

  # } else {
  .pd <- series
  # }
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

validateStreamPD <- function(.stream, verbose) {
  xAxisVariable <- attr(.stream, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('CONTINUOUS','ORDINAL')) {
    stop('The independent axis must be continuous or ordinal for scatterplot.')
  }
  yAxisVariable <- attr(.stream, 'yAxisVariable')
  if (!yAxisVariable$dataShape %in% c('CONTINUOUS')) {
    stop('The dependent axis must be continuous for scatterplot.')
  }
  overlayVariable <- attr(.stream, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL', 'CONTINUOUS')) {
      stop('The overlay variable must be binary, ordinal, categorical, or continuous.')
    }
  }
  facetVariable1 <- attr(.stream, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.stream, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }
  logWithTime('Stream plot request has been validated!', verbose)

  return(.stream)
}

#' Stacked area Plot as data.table
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

#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @export
stream.dt <- function(data, 
                         map, 
                         verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)  

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
    
    logWithTime('Repeated plot references have been melted into a list variable!', verbose)
  } # end handling of repeated plot element references

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type scatter.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    stop("Must provide yAxisVariable for plot type scatter.")
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  if (!is.null(overlayVariable$variableId)) {
    if (overlayVariable$dataShape == 'CONTINUOUS') {
      stop('Continuous overlay variables cannot be used with trend lines.')
    }
  }
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  .stream <- newStreamPD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            verbose = verbose)

  .stream <- validateStreamPD(.stream, verbose)
  logWithTime(paste('New stream plot object created with parameters verbose =', verbose), verbose)

  return(.stream)
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
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @export
stream <- function(data, map, 
                      verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .stream <- stream.dt(data, map, verbose)
  outFileName <- writeJSON(.stream, F, 'stream', verbose)

  return(outFileName)
}

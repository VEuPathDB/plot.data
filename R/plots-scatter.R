newScatterPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
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

  series <- noStatsFacet(.pd, group, panel)
  names(series) <- c(group, panel, 'series.y', 'series.x')

  if (value == 'smoothedMean') {
    interval <- groupSmoothedMean(.pd, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel)
    
    .pd <- interval

  } else if (value == 'smoothedMeanWithRaw') {
    
    interval <- groupSmoothedMean(.pd, x, y, group, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.x', 'interval.y', 'interval.se', group, panel)
    
    if (!is.null(key(series))) {
      .pd <- merge(series, interval)
    } else {
      .pd <- cbind(series, interval)
    }
    
  } else if (value == 'density') {
    density <- groupDensity(.pd, x, group, panel)
    names(density) <- c(group, panel, 'density.x', 'density.y')
    .pd <- density
    
  } else {
    # Return raw data
    .pd <- series
  }
  attr$names <- names(.pd)

  attributes(.pd) <- attr

  return(.pd)
}

validateScatterPD <- function(.scatter) {
  xAxisVariable <- attr(.scatter, 'xAxisVariable')
  if (!xAxisVariable$dataType %in% c('NUMBER','DATE')) {
    stop('The independent axis must be of type number or date for scatterplot.')
  }
  yAxisVariable <- attr(.scatter, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER','DATE')) {
    stop('The dependent axis must be of type number or date for scatterplot.')
  }

  return(.scatter)
}

#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'series.x' and 'series.y' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.x', 
#' 'interval.y' and 'interval.se' specify the x, y and standard error
#'  respectively of the smoothed conditional mean for the group. 
#'  Columns 'density.x' and 'density.y' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates
#' @return data.table plot-ready data
#' @export
scattergl.dt <- function(data, 
                         map, 
                         value = c('smoothedMean', 'smoothedMeanWithRaw', 'density', 'raw')) {
  value <- match.arg(value)

  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)

  if (!'data.table' %in% class(data)) {
    data <- data.table::as.data.table(data)
  }

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  } else {
    stop("Must provide xAxisVariable for plot type scatter.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
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
#' 'series.x' and 'series.y' contain the raw data for the 
#' scatter plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.x', 
#' 'interval.y' and 'interval.se' specify the x, y and standard error
#'  respectively of the smoothed conditional mean for the group.
#'  Columns 'density.x' and 'density.y' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, value = c('smoothedMean', 'smoothedMeanWithRaw', 'density', 'raw')) {
  value <- match.arg(value)
  .scatter <- scattergl.dt(data, map, value)
  outFileName <- writeJSON(.scatter, 'scattergl')

  return(outFileName)
}

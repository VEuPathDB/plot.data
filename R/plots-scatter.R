newScatterPD <- function(.dt = data.table::data.table(),
                         independentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         dependentVar = list('variableId' = NULL,
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
                     independentVar = independentVar,
                     dependentVar = dependentVar,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "scatterplot")

  attr <- attributes(.pd)

  independent <- attr$independentVar$variableId
  dependent <- attr$dependentVar$variableId
  overlay <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  series <- noStatsFacet(.pd, overlay, panel)
  names(series) <- c(overlay, panel, 'series.dependent', 'series.independent')

  if (value == 'smoothedMean') {
    interval <- groupSmoothedMean(.pd, independent, dependent, overlay, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.independent', 'interval.dependent', 'interval.se', overlay, panel)
    
    .pd <- interval

  } else if (value == 'smoothedMeanWithRaw') {
    
    interval <- groupSmoothedMean(.pd, independent, dependent, overlay, panel)
    interval <- interval[, !c('ymin', 'ymax')]
    names(interval) <- c('interval.independent', 'interval.dependent', 'interval.se', overlay, panel)
    
    if (!is.null(key(series))) {
      .pd <- merge(series, interval)
    } else {
      .pd <- cbind(series, interval)
    }
    
  } else if (value == 'density') {
    density <- groupDensity(.pd, independent, overlay, panel)
    names(density) <- c(overlay, panel, 'density.independent', 'density.dependent')
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
  independentVar <- attr(.scatter, 'independentVar')
  if (!independentVar$dataType %in% c('NUMBER','DATE')) {
    stop('The independent axis must be of type number or date for scatterplot.')
  }
  dependentVar <- attr(.scatter, 'dependentVar')
  if (!dependentVar$dataType %in% c('NUMBER','DATE')) {
    stop('The dependent axis must be of type number or date for scatterplot.')
  }

  return(.scatter)
}

#' Scatter Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per overlay (per panel). Columns 
#' 'series.independent' and 'series.dependent' contain the raw data for the 
#' scatter plot. Column 'overlay' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.independent', 
#' 'interval.dependent' and 'interval.se' specify the independent var, dependent var, and standard error
#'  respectively of the smoothed conditional mean for the group. 
#'  Columns 'density.independent' and 'density.dependent' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean
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

  if ('independentVar' %in% map$plotRef) {
    independentVar <- plotRefMapToList(map, 'independentVar')
  } else {
    stop("Must provide independentVar for plot type scatter.")
  }
  if ('dependentVar' %in% map$plotRef) {
    dependentVar <- plotRefMapToList(map, 'dependentVar')
  } else {
    stop("Must provide dependentVar for plot type scatter.")
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
                            independentVar = independentVar,
                            dependentVar = dependentVar,
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
#' plot-ready data with one row per overlay (per panel). Columns 
#' 'series.independent' and 'series.dependent' contain the raw data for the 
#' scatter plot. Column 'overlay' and 'panel' specify the group the 
#' series data belongs to. Optionally, columns 'interval.independent', 
#' 'interval.dependent' and 'interval.se' specify the independent var, dependent var, and standard error
#'  respectively of the smoothed conditional mean for the group.
#'  Columns 'density.independent' and 'density.dependent' contain the calculated kernel 
#'  density estimates.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'independentVar', 'dependentVar', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value character indicating whether to calculate 'smoothedMean' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean
#' @return character name of json file containing plot-ready data
#' @export
scattergl <- function(data, map, value = c('smoothedMean', 'smoothedMeanWithRaw', 'density', 'raw')) {
  value <- match.arg(value)
  .scatter <- scattergl.dt(data, map, value)
  outFileName <- writeJSON(.scatter, 'scattergl')

  return(outFileName)
}

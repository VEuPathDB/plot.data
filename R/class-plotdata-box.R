newBoxPD <- function(.dt = data.table::data.table(),
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
                         points = character(),
                         mean = character(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     class = "boxplot")

  attr <- attributes(.pd)

  x <- attr$xAxisVariable$variableId
  y <- attr$yAxisVariable$variableId
  group <- attr$overlayVariable$variableId
  panel <- findPanelColName(attr$facetVariable1$variableId, attr$facetVariable2$variableId)

  summary <- groupSummary(.pd, x, y, group, panel)
  fences <- groupFences(.pd, x, y, group, panel)
  fences <- fences[, -x, with = FALSE]
  if (!is.null(key(summary))) {
    .pd.base <- merge(summary, fences)
  } else {
    .pd.base <- cbind(summary, fences)
  }

  if (points == 'outliers') {
    points <- groupOutliers(.pd, x, y, group, panel)
    points[[x]] <- NULL
    if (!is.null(key(points))) {
      .pd.base <- merge(.pd.base, points)
    } else {
      .pd.base <- cbind(.pd.base, points)
    }
  } else if (points == 'all') {
    rawData <- collapseByGroup(.pd, group, panel)
    data.table::setnames(rawData, x, 'seriesX')
    data.table::setnames(rawData, y, 'seriesY')

    if (!is.null(key(rawData))) {
      .pd.base <- merge(.pd.base, rawData)
    } else {
      .pd.base <- cbind(.pd.base, rawData)
    }
  }

  if (mean) {
    mean <- groupMean(.pd, x, y, group, panel)
    mean[[x]] <- NULL
    if (!is.null(key(mean))) {
      .pd.base <- merge(.pd.base, mean)
    } else {
      .pd.base <- cbind(.pd.base, mean)
    }
  }
  .pd <- .pd.base
  attr$names <- names(.pd)

  setAttrFromList(.pd, attr)

  return(.pd)
}

validateBoxPD <- function(.box) {
  xAxisVariable <- attr(.box, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
    stop('The independent axis must be binary, ordinal or categorical for boxplot.')
  }
  yAxisVariable <- attr(.box, 'yAxisVariable')
  if (!yAxisVariable$dataType %in% c('NUMBER')) {
    stop('The dependent axis must be of type number for boxplot.')
  }
  overlayVariable <- attr(.box, 'overlayVariable')
  if (!is.null(overlayVariable)) {
    if (!overlayVariable$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The overlay variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable1 <- attr(.box, 'facetVariable1')
  if (!is.null(facetVariable1)) {
    if (!facetVariable1$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The first facet variable must be binary, ordinal or categorical.')
    }
  }
  facetVariable2 <- attr(.box, 'facetVariable2')
  if (!is.null(facetVariable2)) {
    if (!facetVariable2$dataShape %in% c('BINARY', 'ORDINAL', 'CATEGORICAL')) {
      stop('The second facet variable must be binary, ordinal or categorical.')
    }
  }

  return(.box)
}

#' Box Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return data.table plot-ready data
#' @export
box.dt <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }

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
    repeatedPlotRef <- unique(map$plotRef[duplicated(map$plotRef)])
    
    # Only allow one plot element to be a list var 
    if (length(repeatedPlotRef) > 1) {
      stop("Only one plot element can contain multiple vars.")
    }
    
    # Ensure repeatedPlotRef is numeric
    if (any(map$dataType[map$plotRef == repeatedPlotRef] != 'NUMBER')) {
      stop(paste0("All vars in ", repeatedPlotRef, " must be of type NUMBER."))
    }
    
    # Box-specific flows
    #### Currently left un-optimized to ensure we have correct flows. 
    if (repeatedPlotRef == 'xAxisVariable') {
      meltedVarPlotRef <- 'xAxisVariable'
      meltedValuePlotRef <- 'yAxisVariable'
    } else if (repeatedPlotRef == 'facetVariable1') {
      meltedVarPlotRef <- 'facetVariable1'
      meltedValuePlotRef <- 'yAxisVariable'
    } else {
      stop("Incompatable repeated variable")
    }
    
    # Check to ensure meltedValuePlotRef is not already defined
    if (any(map$plotRef == meltedValuePlotRef)) {
      stop(paste0("Cannot melt data: ", meltedValuePlotRef, " already defined."))
    }
    
    # Check to ensure if repeatedPlotRef is facet that there are no other facet vars.
    if (repeatedPlotRef == 'facetVariable1' & any(map$plotRef == 'facetVariable2')) {
      stop("facetVariable2 should be NULL when using repeated var for facetVariable1")
    }
    
    # Record variable order
    repeatedVarIdOrder <- map$id[map$plotRef == repeatedPlotRef]
    
    # Melt data and update the map 
    data <- data.table::melt(data, measure.vars = repeatedVarIdOrder, variable.factor = FALSE, variable.name='meltedVariable', value.name='meltedValue')
    map <- remapVariableList(map, repeatedPlotRef, meltedVarPlotRef, meltedValuePlotRef)
    
  } # end handling of repeated plot element references

  if ('xAxisVariable' %in% map$plotRef) {
    xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  } else {
    stop("Must provide xAxisVariable for plot type box.")
  }
  if ('yAxisVariable' %in% map$plotRef) {
    yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  } else {
    stop("Must provide yAxisVariable for plot type box.")
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

  .box <- newBoxPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points,
                    mean)

  .box <- validateBoxPD(.box)

  return(.box) 

}

#' Box Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
#' pre-computed values per group. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return columns 'outliers' and 'mean' as well.
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param points character vector indicating which points to return 'outliers' or 'all'
#' @param mean boolean indicating whether to return mean value per group (per panel)
#' @return character name of json file containing plot-ready data
#' @export
box <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }
  .box <- box.dt(data, map, points, mean)
  outFileName <- writeJSON(.box, 'boxplot')

  return(outFileName)
}

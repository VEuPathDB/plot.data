"%ni%" <- Negate("%in%")

validateValues <- function(valuesOfInterest, valuesOfVariable) {
  if (any(valuesOfInterest %ni% valuesOfVariable)) {
    stop("Values of interest do not exist as real values of the specified variable.")
  }
}

findViewport <- function(x, xType) {
  if (xType %in% c('NUMBER', 'INTEGER')) {
    viewport <- list('xMin' = min(0,min(x)), 'xMax' = max(x))
  } else {
    viewport <- list('xMin' = min(x), 'xMax' = max(x))
  }

  return(viewport)
}

tableXY <- function(data) {
  table(data$x, data$y)
}

tableAsDT <- function(data, x, y) {
  tbl <- table(data[[x]], data[[y]])
  xLabels <- rownames(tbl)
  yLabels <- colnames(tbl)
  if (is.null(xLabels)) xLabels <- character(0)
  if (is.null(yLabels)) yLabels <- character(0)
  rownames(tbl) <- NULL
  colnames(tbl) <- NULL
  dt <- data.table::data.table('xLabel'=xLabels,'yLabel'=rep(list(yLabels),length(xLabels)),'value'=lapply(apply(tbl,1,list),unlist))
  dt <- collapseByGroup(dt)

  return(dt)
}

collapseByGroup <- function(data, group = NULL, panel = NULL) {
  if (class(data)[1] != "data.table") {
    data <- data.table::setDT(data)
  }

  if (is.null(group) && is.null(panel)) {
    dt <- data[, lapply(.SD, list)]
  } else {
    dt <- data[, lapply(.SD, list), by=eval(colnames(data)[colnames(data) %in% c(group, panel)])]
  }
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

plotRefMapToList <- function(map, plotRef) {
  if (!plotRef %in% map$plotRef) {
    return(list('variableId' = NULL,
                'entityId' = NULL,
                'dataType' = NULL,
                'dataShape' = NULL,
                'displayLabel' = NULL,
                'naToZero' = NULL))
  }

  variableId <- lapply(map$id[map$plotRef == plotRef], veupathUtils::strSplit, ".", 4, 2)
  entityId <- lapply(map$id[map$plotRef == plotRef], veupathUtils::strSplit, ".", 4, 1)

  # If there are no variable
  variableId <- veupathUtils::toStringOrNull(variableId)
  entityId <- veupathUtils::toStringOrNull(entityId)
  dataType <- veupathUtils::toStringOrNull(map$dataType[map$plotRef == plotRef])
  dataShape <- veupathUtils::toStringOrNull(map$dataShape[map$plotRef == plotRef])
  displayLabel <- veupathUtils::toStringOrNull(map$displayLabel[map$plotRef == plotRef])
  naToZero <- veupathUtils::toStringOrNull(map$naToZero[map$plotRef == plotRef])
  
  # Validate naToZero and fix if necessary
  # NOTE failing to set naToZero will result in a default value of FALSE
  if (length(naToZero) == 0) {
    warning("Encountered empty or NULL naToZero value. Setting naToZero = FALSE.")
    naToZero <- FALSE
  } else if (is.na(naToZero) || naToZero == '') {
    warning("Encountered '' or NA as the naToZero value. Setting naToZero = FALSE.")
    naToZero <- FALSE
  } else {
    if (identical(naToZero, 'TRUE')) naToZero <- TRUE
    if (identical(naToZero, 'FALSE')) naToZero <- FALSE
  }
  if (!(naToZero %in% c(TRUE, FALSE))) {stop("plotRefMapToList error: Unrecognized value submitted for naToZero")}

  if (!is.null(variableId) & !is.null(entityId)) {
    if (all(variableId == entityId)) { entityId <- NULL }
  }

  plotRef <- list('variableId' = variableId,
                  'entityId' = entityId,
                  'dataType' = dataType,
                  'dataShape' = dataShape,
                  'displayLabel' = displayLabel,
                  'naToZero' = naToZero)

  return(plotRef)
}

#' @importFrom lubridate is.Date
#' @importFrom lubridate as_date
updateType <- function(x, xType) {
  if (xType %in% c('NUMBER', 'INTEGER') & !is.numeric(x)) { x <- as.numeric(x) }
  if (xType == 'DATE' & !lubridate::is.Date(x)) { x <- lubridate::as_date(x) }
  if (xType == 'STRING' & !is.character(x)) { x <- as.character(x) }

  return(x)
}

getPDAttributes <- function(.pd) {
  attr <- attributes(.pd)
  attr$names <- NULL
  attr$class <- NULL
  attr$row.names <- NULL
  attr$.internal.selfref <- NULL

  return(attr)
}

getInteractionColsList <- function(data, group, panel) {
  if (is.null(panel)) {
    colsList <- list(data[[group]])
  } else {
    if (is.null(group)) {
      colsList <- list(data[[panel]])
    } else {
      colsList <- list(data[[group]], data[[panel]])
    }
  }
  
  return(colsList)  
}

removeGroupPanel <- function(data, group, panel) {
  data[[group]] <- NULL
  data[[panel]] <- NULL
  
  return(data)
}

#' Contingency Table as data.table
#'
#' This function returns a data.table representation of the results
#' from table() 
#' @param data data.table to make contingency table for
#' @return data.table of frequency distribution values
#' @export
contingencyDT <- function(data, labels = TRUE) {
  dt <- as.data.frame.matrix(table(data$x, data$y))
  if (labels) {
    dt$label <- rownames(dt)
  }

  return(data.table::setDT(dt))
}


findPanelColName <- function(facet1 = NULL, facet2 = NULL) {
  if (!is.null(facet1$variableId) & !is.null(facet2$variableId)) {
    panel <- 'panel'
  } else if (!is.null(facet1)) {
    panel <- veupathUtils::toColNameOrNull(facet1)
  } else if (!is.null(facet2)) {
    panel <- veupathUtils::toColNameOrNull(facet2)
  } else {
    panel <- NULL
  }

  return(panel)
}

#' Make Plot Panels
#'
#' This function returns a list where the first entry is a data.table
#' with one column representing a list of plot panels and the second 
#' entry is the name of the column specifying the plot panels.
#' @param data data.table to make plot panels for
#' @param facet1 name of a column in data to find interaction for
#' @param facet2 name of a column in data to find interaction for
#' @return list of length 2: list(data, panel)
#' @export
makePanels <- function(data, facet1 = NULL, facet2 = NULL) {
  if (!is.null(facet1) & !is.null(facet2)) {
    data$panel <- as.character(interaction(data[[facet1]], data[[facet2]], sep='.||.'))
    if (facet1 != 'panel') {
      data[[facet1]] <- NULL
    }
    if (facet2 != 'panel') {
      data[[facet2]] <- NULL
    }
    panel <- 'panel'
  } else if (!is.null(facet1)) {
    panel <- facet1
  } else if (!is.null(facet2)) {
    panel <- facet2
  } else {
    panel <- NULL
  }

  return(list(data,panel))
}

#' Adjust Data Range to Viewport
#' 
#'
#' This function will adjust the range of numeric vector `x` 
#' the values in ‘x’ according to which interval they fall
#' @param x Numeric or Date vector to bin
#' @param binWidth number to increment bin bounds by, or string for dates ex: 'month'
#' @return Character vector of coded values 
#' @export
adjustToViewport <- function(x, viewport) {
  if (is.null(viewport)) { return(x) }  

  # current R/bin.R subsets, and this will only ever have to expand
  if (viewport$xMin < min(x)) {
    x <- c(viewport$xMin, x)
  } else {
    x <- x[x >= viewport$xMin]
  }
  if (viewport$xMax > max(x)) {
    x <- c(x, viewport$xMax)
  } else {
    x <- x[x <= viewport$xMax]
  }

  return(x)
}

pruneViewportAdjustmentFromBins <- function(bins, xVP, x, viewport) {
  if (viewport$xMin < min(x)) {
    bins <- bins[xVP != viewport$xMin]
    xVP <- xVP[xVP != viewport$xMin]
  }
  if (viewport$xMax > max(x)) {
    bins <- bins[xVP != viewport$xMax]
  }

  return(bins)
}

findBinStart <- function(x) {
  if (!length(x)) {
     return(character(0))
  }

  if (all(grepl(" - ",x))) {
    x <- veupathUtils::strSplit(x, " - ")
    x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\(|\\[", "", veupathUtils::strSplit(as.character(x), ","))
  }

  #try to infer type. may need more robust solution  
  if (!any(is.na(as.numeric(x)))) {
    x <- as.numeric(x)
  }

  return(x)
}

findBinEnd <- function(x) {
  if (!length(x)) {
    return(character(0))
  }

  if (all(grepl(" - ",x))) {
    x <- veupathUtils::strSplit(x, " - ", index = 2)
    x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\)|\\]", "", veupathUtils::strSplit(as.character(x), ",", index = 2))
  }

  return(x)
}

validatecollectionVar <- function(collectionVariable) {

  # Require all repeated vars to have the same type, shape, and entity
  if (length(unique(collectionVariable$entityId)) > 1 | length(unique(collectionVariable$dataType)) > 1 | length(unique(collectionVariable$dataShape)) > 1) {
    stop("collectionVar error: all vars in a collectionVar must have the same entity id, type, and shape.")
  }

  # Ensure all variables are numbers
  if (!all(collectionVariable$dataType %in% c('NUMBER', 'INTEGER'))){
    stop("collectionVar error: All vars must be of type NUMBER or INTEGER.")
  }

  # Ensure all variables are continuous
  if (!all(collectionVariable$dataShape == 'CONTINUOUS')){
    stop("collectionVar error: All vars must be CONTINUOUS.")
  }

  # Ensure no to variables are the same
  if (any(duplicated(collectionVariable$variableId))) {
    stop("collectionVar error: No duplicate vars allowed.")
  }

  return(collectionVariable)
}

validateMap <- function(map) {
  # Could add checks for data type, shape, etc presence

  if (any(duplicated(map$plotRef))) {
    
    # Check that there is at most one repeated plotRef
    repeatedPlotRef <- unique(map$plotRef[duplicated(map$plotRef)])

    if (length(repeatedPlotRef) > 1) {
      stop("map error: only one plotRef can be assigned to multiple variables.")
    }

    # Check we do not have too many repeated variables.
    nVars <- sum(duplicated(map$plotRef))

    if (repeatedPlotRef == 'xAxisVariable' & nVars > 10) {
      stop("Too many values specified with collectionVar: maximum number of x axis values is 10.")
    } else if (repeatedPlotRef == 'overlayVariable' & nVars > 8) {
      stop("Too many values specified with collectionVar: maximum number of overlay values is 8.")
    } else if (repeatedPlotRef == 'facetVariable1' & nVars > 25) {
      stop("Too many values specified with collectionVar: maximum number of facet1 values allowed is 25.")
    } else if (repeatedPlotRef == 'facetVariable2' & nVars > 25) {
      stop("Too many values specified with collectionVar: maximum number of facet2 values allowed is 25.")
    }
  }

  return(map)
}

toIdOrDisplayLabel <- function(colName, plotRef) {
      varIndex <- which(veupathUtils::toColNameOrNull(plotRef) == colName)
      if (is.null(plotRef$displayLabel[varIndex]) || identical(plotRef$displayLabel[varIndex], '')) {
        name <- plotRef$variableId[varIndex]
      } else {
        name <- plotRef$displayLabel[varIndex]
      }
      return(name)
    }


#' @importFrom purrr map
findColNamesByPredicate <- function(variableList, predicateFunction) {

  # For each variable in the variable list, return the column name if the predicate is true for that variable
  colNames <- purrr::map(variableList, function(x) {if (identical(predicateFunction(x), TRUE)) {return(veupathUtils::toColNameOrNull(x))}})
  colNames <- unlist(colNames)

  return (colNames)
}


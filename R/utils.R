recodeOverlayValues <- function(overlayData, desiredOverlayValues, variables) {
  if (is.null(desiredOverlayValues)) return(overlayData)
  dataType <- veupathUtils::findDataTypesFromPlotRef(variables, 'overlay')
  
  if (dataType %in% c('NUMBER', 'INTEGER', 'DATE')) {
    # figure this out when i get to the continuous overlay PR, just leaving a skeleton for now
    # tbh desiredOverlayValues will probably stop being a simple character vector then as well. but one thing at a time.
    warning("Binned continuous overlays are not supported yet.")
  } else {
    if (unique(overlayData) == desiredOverlayValues) return(overlayData)
    overlayData[!overlayData %in% desiredOverlayValues] <- 'All un-selected values'
  }

  return(overlayData)
}


"%ni%" <- Negate("%in%")

validateValues <- function(valuesOfInterest, valuesOfVariable) {
  if (any(valuesOfInterest %ni% valuesOfVariable)) {
    warning("At least one value of interest does not exist as a real value in the specified variable.")
  }
  if (all(valuesOfInterest %ni% valuesOfVariable)) {
    stop("No supplied values of interest exist as real values in the specified variable.")
  }
}

#' @export
findViewport <- function(x, xType) {
  viewport <- list('xMin' = min(x), 'xMax' = max(x))

  return(viewport)
}

findGeolocationViewport <- function(.dt, latitude, longitude) {
  if (any(is.null(c(latitude, longitude)))) {
    viewport <- NULL
  } else {
    nrows <- nrow(.dt)
    viewport <- list('latitude'=list('xMin'=min(.dt[[latitude]]),
                                   'xMax'=max(.dt[[latitude]])),
                   'longitude'=list('left'=min(.dt[[longitude]]),
                                    'right'=max(.dt[[longitude]])))
    # if applying the viewport back to the data filters rows then we have the wrong 'half' the globe 
    if (nrows != nrow(filterToGeolocationViewport(.dt, latitude, longitude, viewport))) {
      viewport$longitude=list('left'=max(.dt[[longitude]]),
                              'right'=min(.dt[[longitude]]))
    }                                
  }

  return(viewport)
}

tableXY <- function(data) {
  table(data$y, data$x)
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

collapseByGroup <- function(data, group = NULL, panel = NULL, geo = NULL) {
  if (class(data)[1] != "data.table") {
    data <- data.table::setDT(data)
  }

  if (all(is.null(c(group, geo, panel)))) {
    dt <- data[, lapply(.SD, list)]
  } else {
    dt <- data[, lapply(.SD, list), by=eval(colnames(data)[colnames(data) %in% c(group, geo, panel)])]
  }
  indexCols <- c(panel, geo, group)
  setkeyv(dt, indexCols)

  return(dt)
}

plotRefMapToList <- function(map, plotRef, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

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
    veupathUtils::logWithTime("Encountered empty or NULL naToZero value. Setting naToZero = FALSE.", verbose)
    naToZero <- FALSE
  } else if (is.na(naToZero) || naToZero == '') {
    veupathUtils::logWithTime("Encountered '' or NA as the naToZero value. Setting naToZero = FALSE.", verbose)
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

setGeneric("findPanelColName", 
  function(facet1VarSpec, facet2VarSpec) standardGeneric("findPanelColName"),
  signature = c("facet1VarSpec", "facet2VarSpec")
)

setMethod("findPanelColName", signature("NULL", "VariableSpec"), function(facet1VarSpec, facet2VarSpec) {
  return(veupathUtils::getColName(facet2VarSpec))
})

setMethod("findPanelColName", signature("VariableSpec", "NULL"), function(facet1VarSpec, facet2VarSpec) {
  return(veupathUtils::getColName(facet1VarSpec))
})

setMethod("findPanelColName", signature("VariableSpec", "VariableSpec"), function(facet1VarSpec, facet2VarSpec) {
  return('panel')
})

setMethod("findPanelColName", signature("NULL", "NULL"), function(facet1VarSpec, facet2VarSpec) {
  return(NULL)
})

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
#' This function will adjust a numeric vector `x` 
#' according to the values specified in the viewport.
#' @param x Numeric or Date vector to adjust
#' @param viewport List indicating viewport min and max values
#' @return Numeric or Date vector of adjusted values 
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

#' Filter Data Table to Geolocation Viewport
#' 
#'
#' This function will filter a data.table to the specified 
#' latitude and longitude values.
#' 
#' @section Geolocation Viewport Structure:
#' This is a list of lists taking the form: \cr
#' *latitude \cr
#' **xMin = numeric \cr
#' **xMax = numeric \cr
#' *longitude \cr
#' **left = numeric \cr
#' **right = numeric \cr
#' @param .dt data.table with columns for latitude and longitude
#' @param latitude Character vector specifying name of latitude column
#' @param longitude Character vector specifying name of longitude column
#' @param viewport List of lists specifying the geolocation viewport.  
#' @return data.table of filtered values 
#' @export
filterToGeolocationViewport <- function(.dt, latitude, longitude, viewport) {
  if (is.null(viewport)) { return(.dt) }  

  .dt <- .dt[.dt[[latitude]] >= viewport$latitude$xMin & .dt[[latitude]] <= viewport$latitude$xMax]
  if (viewport$longitude$left < viewport$longitude$right) {
    .dt <- .dt[.dt[[longitude]] >= viewport$longitude$left & .dt[[longitude]] <= viewport$longitude$right]
  } else {
    .dt <- .dt[.dt[[longitude]] >= viewport$longitude$left | .dt[[longitude]] <= viewport$longitude$right]
  }

  return(.dt)
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

findBinStart <- function(x, addTimeZone = c(FALSE, TRUE)) {
  addTimeZone <- veupathUtils::matchArg(addTimeZone)
  if (!length(x)) {
     return(character(0))
  }

  if (all(grepl(" - ",x))) {
    x <- veupathUtils::strSplit(x, " - ")
    if (addTimeZone) x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\(|\\[", "", veupathUtils::strSplit(as.character(x), ",\\s*", fixed = FALSE))
  }

  #try to infer type. may need more robust solution  
  if (!any(is.na(as.numeric(x)))) {
    x <- as.numeric(x)
  }

  return(x)
}

findBinEnd <- function(x, addTimeZone = c(FALSE, TRUE)) {
  addTimeZone <- veupathUtils::matchArg(addTimeZone)
  if (!length(x)) {
    return(character(0))
  }

  if (all(grepl(" - ",x))) {
    x <- veupathUtils::strSplit(x, " - ", index = 2)
    if (addTimeZone) x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\)|\\]", "", veupathUtils::strSplit(as.character(x), ",\\s*", index = 2, fixed = FALSE))
  }

  return(x)
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

avgDigits <- function(x) {
  floor(mean(stringi::stri_count_regex(as.character(x), "[[:digit:]]")))
}


#
# For any number, return an absolute delta (numeric) at the last
# significant digit in the number, using the number of digits specified
#
# e.g. assuming 3 significant digits
# 
# 1.23 -> 0.01
# 11.0 -> 0.1
# 12.3 -> 0.1
# 101000 -> 1000
# 1.20e-05 -> 0.01e-05 == 1.0e-07
# 0.0123e-05 -> 0.0001e-05 == 1.0e-09
# -2.34e-02 -> 0.01e-02 == 1.0e-04
# 
signifDigitEpsilon <- function(x, digits) {

  # '#' flag ensures trailing zeroes
  # take abs() here because we don't care about sign
  rounded <- formatC(abs(x), digits = digits, width = 1L, flag = '#')

  # split into vector of single characters
  characters <- strsplit(rounded, '')

  result <- c()
  seenSignificant <- FALSE
  significantCount <- 0
  # walk through string, looking for first non-zero, non decimal point character
  for (c in unlist(characters)) {
    if (!(c %in% c('0', '.'))) {
      seenSignificant <- TRUE
    }
    if (c == '.') {
      result <- c(result, c)
    } else if (seenSignificant) {
      significantCount <- significantCount + 1
      if (significantCount < digits) {
        result <- c(result, '0')
      } else if (significantCount == digits) {
        result <- c(result, '1')
      } else {
        # we're out of the significant digits
        # we must be in the exponent part (if present) or in trailing zeroes (e.g. in 101000 example)
        # so just copy it over
        result <- c(result, c)
      }
    } else {
      result <- c(result, '0')
    }
  }

  # return joined result as a number
  as.numeric(paste(result, collapse=""))
}
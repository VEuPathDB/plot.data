nonZeroRound <- function(x, digits) {
  if (x == 0) {
    warning("Input is already zero and cannot be rounded to a non-zero number.")
    return(x)
  }
  if (round(x,digits) == 0) { 
    Recall(x,digits+1) 
  } else { 
    round(x,digits) 
  } 
}

#' Diagnositc Messages with Time of Occurance
#'
#' This function generates a diagnositc message which
#' includes the time of occurance.
#' @param message character to pass to `message`
#' @param verbose boolean indicating if timed logging is desired
#' @export
logWithTime <- function(message, verbose) {
  if (verbose) {
    message('\n', Sys.time(), ' ', message)
  }
}

#' Try-error Test
#'
#' This function returns a logical value indicating if x is
#' a try-error object.
#' @param x an R object
#' @return logical TRUE if x is a try-error object, FALSE otherwise
#' @export
is.error <- function(x) inherits(x, "try-error")

tableXY <- function(data) {
  table(data$x, data$y)
}

tableAsDT <- function(data, x, y) {
  tbl <- table(data[[x]], data[[y]])
  xLabels <- rownames(tbl)
  yLabels <- colnames(tbl)
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

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

plotRefMapToList <- function(map, plotRef) {
  if (!plotRef %in% map$plotRef) {
    return(list('variableId' = NULL,
                'entityId' = NULL,
                'dataType' = NULL,
                'dataShape' = NULL,
                'displayLabel' = NULL))
  }

  variableId <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 2)
  entityId <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 1)

  # If there are no variable
  variableId <- emptyStringToNull(variableId)
  entityId <- emptyStringToNull(entityId)
  dataType <- emptyStringToNull(map$dataType[map$plotRef == plotRef])
  dataShape <- emptyStringToNull(map$dataShape[map$plotRef == plotRef])
  displayLabel <- emptyStringToNull(map$displayLabel[map$plotRef == plotRef])

  if (!is.null(variableId) & !is.null(entityId)) {
    if (all(variableId == entityId)) { entityId <- NULL }
  }

  plotRef <- list('variableId' = variableId,
                  'entityId' = entityId,
                  'dataType' = dataType,
                  'dataShape' = dataShape,
                  'displayLabel' = displayLabel)

  return(plotRef)
}

#' @importFrom lubridate is.Date
#' @importFrom lubridate as_date
updateType <- function(x, xType, xShape='') {
  if (xType %in% c('NUMBER', 'INTEGER') & xShape != 'CATEGORICAL' & !is.numeric(x)) { x <- as.numeric(x) }
  if (xType %in% c('NUMBER', 'INTEGER') & xShape == 'CATEGORICAL' & !is.character(x)) { x <- as.character(x) }
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

#' POSIXct Test
#'
#' This function returns a logical value indicating if x is
#' a POSIXct object.
#' @param x an R object
#' @return logical TRUE if x is a POSIXct object, FALSE otherwise
#' @export
is.POSIXct <- function(x) inherits(x, "POSIXct")

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
    panel <- toColNameOrNull(facet1)
  } else if (!is.null(facet2)) {
    panel <- toColNameOrNull(facet2)
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

emptyStringToPoint <- function(x) {
  if (length(x) == 0) { return(".") }
  if (x == "") { return(".") }

  return(x)
}

#' Replace Empty String with NULL
#'
#' This function replaces the empty string "" with NULL 
#' @param x character vector
#' @return non-empty character vector or NULL
#' @export
emptyStringToNull <- function(x) {
  x <- unlist(x)
  if (is.null(x)) { return(NULL) }
  if (length(x) == 0) { return(NULL) }
  if (all(x == "")) { return(NULL) }

  return(as.character(x))
}

toColNameOrNull <- function(varDetailsList) {
  if (is.null(varDetailsList)) {
    return(NULL)
  }

  if (is.null(varDetailsList$variableId)) {
    return(NULL)
  }

  if (is.null(varDetailsList$entityId)) {
    return(varDetailsList$variableId)
  }

  return(paste0(varDetailsList$entityId, ".", varDetailsList$variableId))
}

getAggStr <- function(numericVars, groupingVars) {
  numericString <- emptyStringToPoint(paste(numericVars, collapse= " + "))
  groupingString <- emptyStringToNull(paste(groupingVars, collapse=" + "))
  aggStr <- paste(c(numericString, groupingString), collapse=" ~ ")

  return(aggStr)
}

# should switch to data.table for consistency
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      stop("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

data_frame <- function(...) {
  new_data_frame(list(...))
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

strSplit <- function(str, pattern, ncol = 2, index = 1, fixed = TRUE) {
  matrix(unlist(strsplit(str, pattern, fixed = fixed)), ncol = ncol, byrow = TRUE)[,index]
}

findBinStart <- function(x) {
  if (all(grepl(" - ",x))) {
    x <- strSplit(x, " - ")
    x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\(|\\[", "", strSplit(as.character(x), ","))
  }

  #try to infer type. may need more robust solution  
  if (!any(is.na(as.numeric(x)))) {
    x <- as.numeric(x)
  }

  return(x)
}

findBinEnd <- function(x) {
  if (all(grepl(" - ",x))) {
    x <- strSplit(x, " - ", index = 2)
    x <- paste0(x,'T00:00:00')
  } else {
    x <- gsub("\\)|\\]", "", strSplit(as.character(x), ",", index = 2))
  }

  return(x)
}

# Set object attributes from a list
setAttrFromList <- function(.dt, attr, removeExtraAttrs=T) {
  
  
  if (!is.data.table(.dt)) {
    stop(".dt must be of class data.table")
  }
  
  # If removeExtraAttrs=T, remove any .dt attribute not in attr
  if (removeExtraAttrs) {
    attrNames <- names(attributes(.dt))
    attrToRemove <- attrNames[!(attrNames %in% names(attr))]
    
    if (length(attrToRemove) > 0) {
      invisible(lapply(attrToRemove, removeAttr, .dt))
    }
  }
  
  # For each item in the attr list, add to .dt attributes or update existing
  invisible(lapply(seq_along(attr), updateAttrById, attr, .dt))
  
  return(.dt)
}

removeAttr <- function(attrToRemove, .dt) {
  data.table::setattr(.dt, attrToRemove, NULL)
  return(NULL)
}

updateAttrById <- function(attrInd, attr, .dt) {
  data.table::setattr(.dt, names(attr)[attrInd], attr[[attrInd]])
  return(NULL)
}


#' Character and Logical Argument Verification
#'
#' `matchArg` matches `arg` against a table of candidates values as
#' specified by `choices`, where `NULL` means to take the first one.
#'
#' In the one-argument form `matchArg(arg)`, the choices are
#' obtained from a default setting for the formal argument `arg` of
#' the function from which `matchArg` was called.  (Since default
#' argument matching will set `arg` to `choices`, this is allowed as
#' an exception to the "length one unless `several.ok` is `TRUE`"
#' rule, and returns the first element.)
#' @param arg a character vector of length one
#' @param choices a character vector of candidate values
#' @return The unabbreviated version of the exact match
#' @importFrom  stringi stri_detect_regex
#' @export
matchArg <- function(arg, choices) {
  
  # If choices is not supplied, extract from function definition
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]])
  }

  # Return first value as default
  if (is.null(arg)) return(choices[1L])
  if (identical(arg, choices)) return(arg[1L])

  # Validate inputs
  if (!identical(typeof(arg), typeof(choices))) {
    stop("'arg' must be of the same type as 'choices'.")
  }
  if (length(arg) != 1L) stop("'arg' must be of length 1")
  if (!is.character(arg) && !is.logical(arg)) {
     stop("'arg' must be NULL, a character vector, or a logical vector.")
  }
  
  # Perform argument matching based on type
  if (is.character(arg)) {

    # If arg does not match any values in choices, err. Otherwise, arg must have matched.
    if (!any(stringi::stri_detect_regex(choices, paste0('^', arg, '$')))) {
      stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), collapse = ", ")), domain = NA)
    }
    
  } else if (is.logical(arg)) {

    # If arg does not match any values in choices, err. Otherwise, arg must have matched.
    if (!(arg %in% choices)) {
      stop("'arg' does not match any value in 'choices'")
    }
  }

  return (arg)
}
  

# remapListVar <- function(map, listVarPlotRef, newValuePlotRef, newVarId = 'meltedVariable', newValueId = 'meltedValue', newVarDisplayLabel = NULL, newValueDisplayLabel = NULL) {
  
#   listVarEntity <- unique(map$entityId[map$plotRef == listVarPlotRef])
#   listVarType <- unique(map$dataType[map$plotRef == listVarPlotRef])
#   listVarShape <- unique(map$dataShape[map$plotRef == listVarPlotRef])

#   newVar <- list('id' = newVarId, 'plotRef' = listVarPlotRef)
#   newValue <- list('id' = newValueId, 'plotRef' = newValuePlotRef)
#   if (!is.null(listVarEntity)) {
#     newVar$entityId <- listVarEntity
#     newValue$entityId <- listVarEntity
#   }
#   if (!is.null(listVarType)) {
#     newVar$dataType <- 'STRING'
#     newValue$dataType <- listVarType
#   }
#   if (!is.null(listVarShape)) {
#     newVar$dataShape <- 'CATEGORICAL'
#     newValue$dataShape <- listVarShape
#   }

#   # Add displayLabels
#   if (!is.null(map$displayLabel)) {
#     newVar$displayLabel <- if(!is.null(newVarDisplayLabel)) {newVarDisplayLabel} else {''}
#     newValue$displayLabel <- if(!is.null(newValueDisplayLabel)) {newValueDisplayLabel} else {''}
#   }
  
#   # Remove all repeated variables from map
#   map <- map[!(map$plotRef == listVarPlotRef), ]
  
#   # Add new variables
#   map <- rbind(map, newVar)
#   map <- rbind(map, newValue)
  
#   return(map)
# }


validateListVar <- function(listVariable) {

  # Require all repeated vars to have the same type, shape, and entity
  if (length(unique(listVariable$entityId)) > 1 | length(unique(listVariable$dataType)) > 1 | length(unique(listVariable$dataShape)) > 1) {
    stop("listVar error: all vars in a listVar must have the same entity id, type, and shape.")
  }

  # Ensure all variables are numbers
  if (!all(listVariable$dataType %in% c('NUMBER', 'INTEGER'))){
    stop("listVar error: All vars must be of type NUMBER or INTEGER.")
  }

  # Ensure all variables are continuous
  if (!all(listVariable$dataShape == 'CONTINUOUS')){
    stop("listVar error: All vars must be CONTINUOUS.")
  }

  # Ensure no to variables are the same
  if (any(duplicated(listVariable$variableId))) {
    stop("listVar error: No duplicate vars allowed.")
  }

  return(listVariable)
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
      stop("Too many values specified with listVar: maximum number of x axis values is 10.")
    } else if (repeatedPlotRef == 'overlayVariable' & nVars > 8) {
      stop("Too many values specified with listVar: maximum number of overlay values is 8.")
    } else if (repeatedPlotRef == 'facetVariable1' & nVars > 25) {
      stop("Too many values specified with listVar: maximum number of facet1 values allowed is 25.")
    } else if (repeatedPlotRef == 'facetVariable2' & nVars > 25) {
      stop("Too many values specified with listVar: maximum number of facet2 values allowed is 25.")
    }
  }

  return(map)
}

toIdOrDisplayLabel <- function(colName, plotRef) {
      varIndex <- which(toColNameOrNull(plotRef) == colName)
      if (is.null(plotRef$displayLabel[varIndex]) || identical(plotRef$displayLabel[varIndex], '')) {
        name <- plotRef$variableId[varIndex]
      } else {
        name <- plotRef$displayLabel[varIndex]
      }
      return(name)
    }


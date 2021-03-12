makeVariableDetails <- function(value, variableId, entityId) {
  if (!is.null(value)) {
    variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=jsonlite::unbox(value)) 
  } else {
    variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId))
  }

  return(variableDetails)
}

addStrataVariableDetails <- function(data) {
  namedAttrList <- attributes(data)
  group <- ifelse('overlayVariable' %in% names(namedAttrList), namedAttrList$overlayVariable$variableId, NULL) 
  facet1 <- ifelse('facetVariable1' %in% names(namedAttrList), namedAttrList$facetVariable1$variableId, NULL)
  facet2 <- ifelse('facetVariable2' %in% names(namedAttrList), namedAttrList$facetVariable2$variableId, NULL)
 
  # TODO test w two facets. not sure this will work..
  if ('panel' %in% names(data)) {
    names(data)[names(data) == 'panel'] <- 'facetVariableDetails'
    data$facetVariableDetails <- lapply(data$facetVariableDetails, makeVariableDetails, list(facet1,facet2), map$entityId[map$id %in% c(facet1, facet2)])
  } else if (!is.null(group)) {
    names(data)[names(data) == group] <- 'overlayVariableDetails'
    data$overlayVariableDetails <- lapply(data$overlayVariableDetails, makeVariableDetails, group, namedAttrList$overlayVariable$entityId)
  } else if (!is.null(facet1)) {
    names(data)[names(data) == facet1] <- 'facetVariableDetails'
    data$facetVariableDetails <- lapply(data$facetVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId)
  } else if (!is.null(facet2)) {
    names(data)[names(data) == facet2] <- 'facetVariableDetails'
    data$facetVariableDetails <- lapply(data$facetVariableDetails, makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId)
  }

  return(data)
}

#' Write json to local tmp file
#'
#' This function returns the name of a json file which it has
#' written a data.table object out to.
#' @param data a data.table to convert to json and write to a tmp file
#' @param pattern optional tmp file prefix
#' @return character name of a tmp file w ext *.json
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite prettify
#' @export
writeJSON <- function(data, pattern = NULL) {
  namedAttrList <- getPDAttributes(data)

  #TODO consider if this is something plot.data objects can do for themselves ?
  if (any(c('overlayVariable', 'facetVariable1', 'facetVariable2') %in% names(namedAttrList))) {
    data <- addStrataVariableDetails(data)
  }

  #TODO think of better way to do this reformatting
  if ('xAxisVariable' %in% names(namedAttrList)) {
    xVariableDetails <- list('xVariableDetails' = makeVariableDetails(NULL, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId))
    namedAttrList$xAxisVariable <- NULL
    namedAttrList$xVariableDetails <- xVariableDetails
  }
  if ('yAxisVariable' %in% names(namedAttrList)) {
    yVariableDetails <- list('yVariableDetails' = makeVariableDetails(NULL, namedAttrList$yAxisVariable$variableId, namedAttrList$yAxisVariable$entityId))
    namedAttrList$yAxisVariable <- NULL
    namedAttrList$yVariableDetails <- yVariableDetails
  }
  if ('zAxisVariable' %in% names(namedAttrList)) {
    zVariableDetails <- list('zVariableDetails' = makeVariableDetails(NULL, namedAttrList$zAxisVariable$variableId, namedAttrList$zAxisVariable$entityId))
    namedAttrList$zAxisVariable <- NULL
    namedAttrList$zVariableDetails <- zVariableDetails
  }

  outJson <- jsonlite::toJSON(list('data'=data, 'config'=namedAttrList))

  # just for now for debugging
  outJson <- jsonlite::prettify(outJson)
  if (is.null(pattern)) { pattern <- 'file' }
  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)

  return(outFileName)
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

  return(data.table::as.data.table(dt))
}

findPanelColName <- function(facet1 = NULL, facet2 = NULL) {
  if (!is.null(facet1) & !is.null(facet2)) {
    panel <- 'panel'
  } else if (!is.null(facet1)) {
    panel <- facet1
  } else if (!is.null(facet2)) {
    panel <- facet2
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
    data$panel <- interaction(data[[facet1]], data[[facet2]])
    data[[facet1]] <- NULL
    data[[facet2]] <- NULL
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
  if (length(x) == 0) { return(NULL) }
  if (x == "") { return(NULL) }

  return(x)
}

getAggStr <- function(numericVars, groupingVars) {
  numericString <- emptyStringToPoint(paste(numericVars, collapse= " + "))
  groupingString <- emptyStringToNull(paste(groupingVars, collapse=" + "))
  aggStr <- paste(c(numericString, groupingString), collapse=" ~ ")

  return(aggStr)
}

#' Fences
#'
#' This function returns the lower and upper fences for a numeric
#' vector. The lower fence is calculated as the smallest value above `q1 - 1.5*IQR` 
#' and the upper as the largest value below `q3 + 1.5*IQR`.
#' @param x Numeric vector to calculate fences for
#' @return Numeric vector of length 2: c(lowerfence, upperfence)
#' @export
fences <- function(x) {
  summary <- stats::quantile(x)
  iqr <- summary[4] - summary[2]
  lowerfence <- summary[2] - (1.5*iqr)
  lowerfence <- min(x[x > lowerfence])
  upperfence <- summary[4] + (1.5*iqr)
  upperfence <- max(x[x < upperfence])

  return(c(lowerfence, upperfence))
}

#' Outliers
#'
#' This function returns those entries which fall outside the lower 
#' and upper fences given a numeric vector. The lower fence is 
#' calculated as `q1 - 1.5*IQR` and the upper as `q3 + 1.5*IQR`.
#' @param x Numeric vector to identify outliers for
#' @return Numeric vector of outliers
#' @export
outliers <- function(x) {
  fences <- fences(x)

  return(x[x < fences[1] | x > fences[2]])
}

#' Density Curve
#'
#' This function computes gaussian kernal density estimates. 
#' The kernels are scaled such that the bandwidth is the standard 
#' deviation of the smoothing kernel.  
#' @param x Numeric vector to calculate smoothed density estimates for
#' @return data.table with two columns: x) the coordinates of the points where the density is estimated and y) the estimated density values. These will be non-negative, but can be zero. 
#' @export
#' @import data.table
densityCurve <- function(x) {
  curve <- stats::density(x)

  return(data.table::data.table("x" = c(curve$x), "y" = c(curve$y)))
}

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
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

# Prediction data frame
# Get predictions with standard errors into data frame
#
# @keyword internal
# @alias predictdf.gam
# @alias predictdf.loess
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

predictdf.loess <- function(model, xseq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(x = xseq), se = se)

  if (se) {
    y = pred$fit
    ci <- pred$se.fit * stats::qt(level / 2 + .5, pred$df)
    ymin = y - ci
    ymax = y + ci
    base::data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    base::data.frame(x = xseq, y = as.vector(pred))
  }
}

predictdf.gam <- function(model, xseq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(x = xseq), se.fit = se,
    type = "link")

  if (se) {
    std <- stats::qnorm(level / 2 + 0.5)
    base::data.frame(
      x = xseq,
      y = model$family$linkinv(as.vector(pred$fit)),
      ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se.fit)),
      ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se.fit)),
      se = as.vector(pred$se.fit)
    )
  } else {
    base::data.frame(x = xseq, y = model$family$linkinv(as.vector(pred)))
  }
}

#' Smoothed Conditional Mean
#'
#' This function returns for every x value the predicted mean, 95% confidence interval and standard error.
#' Calculation is performed by the (currently undocumented)
#' `predictdf()` generic and its methods. `loess`, uses a t-based 
#' approximation, and for `gam` the normal confidence interval is 
#' constructed on the link scale and then back-transformed to 
#' the response scale.
#' @param dt data.frame with at least two columns: x) representing the independent variable and y) representing the dependent variable
#' @param method Character string one of 'loess' or 'gam'
#' @return data.table with the follwing columns x, y (predicted mean), ymin, ymax (lower and upper bound of confidence interval), se (standard error) 

#' @export
#' @importFrom mgcv gam
smoothedMean <- function(dt, method) {
  xseq <- sort(unique(dt$x))

  if (method == 'loess') {
    smoothed <- stats::loess(y ~ x, dt)
  } else if (method == 'gam') {
    smoothed <- mgcv::gam(y ~ s(x, bs = "cs"), data = dt, method = "REML")
  } else {
    stop('Unrecognized smoothing method.')
  }

  smoothed <- data.table::as.data.table(predictdf(smoothed, xseq))

  return(data.table::data.table("x" = list(smoothed$x), "y" = list(smoothed$y), "ymin" = list(smoothed$ymin), "ymax" = list(smoothed$ymax), "se" = list(smoothed$se)))
}

findNumBins <- function(x, viewport) {
  bins <- bin(x, viewport)
  
  return(data.table::uniqueN(bins))
}

#' Calculate Bin Width
#' 
#' This function determines the ideal bin width based on the range,
#' sample size and distribution of values.
#' @param x Numeric or Date vector
#' @return Numeric or character bin width
#' @export
# @alias findBinWidth.numeric
# @alias findBinWidth.POSIXct
findBinWidth <- function(x, viewport) UseMethod("findBinWidth")

findBinWidth.numeric <- function(x, viewport) {
  numBins <- findNumBins(x, viewport)
  range <- as.numeric(max(x) - min(x))
  range <- range + (range*.01)
  binWidth <- range / numBins
      
  return(binWidth)
}

#TODO make sure it works w units other than days
findBinWidth.POSIXct <- function(x, viewport) {
  dateMap <- data.table('date' = x, 'numeric' = as.numeric(x))
  numBins <- findNumBins(dateMap$numeric, viewport)
  range <- as.numeric(max(dateMap$date) - min(dateMap$date))
  binWidth <- range / numBins
  
  if (binWidth > 365) {
    binWidth <- "year"
  } else if (binWidth > 31 ) {
    binWidth <- "month"
  } else if (binWidth > 7) {
    binWidth <- "week"
  } else {
    binWidth <- "day"
  }
  
  return(binWidth)
}

#' Binning
#' 
#'
#' This function divides the range of ‘x’ into intervals and codes 
#' the values in ‘x’ according to which interval they fall
#' @param x Numeric or Date vector to bin
#' @param binWidth number to increment bin bounds by, or string for dates ex: 'month'
#' @return Character vector of coded values 
#' @export
#' @importFrom lubridate ceiling_date
#' @importFrom varrank discretization
#' @importFrom moments skewness
# @alias bin.numeric
# @alias bin.POSIXct
bin <- function(x, binWidth, viewport) UseMethod("bin")

bin.numeric <- function(x, binWidth = NULL, viewport) {
  bins <- NULL
  myMethod <- NULL

  # TODO consider if we are meant to choose method before or 
  # after the viewport adjustment 
  if (is.null(binWidth)) {
    if (length(x) < 200) {
      myMethod <- 'sturges'
    }
    skewness <- moments::skewness(x)
    if (abs(skewness) > .5) {
      myMethod <- 'doane'
    }
    if (is.null(bins)) {
      myMethod <- 'fd'
    }
  }

  addViewportMin <- FALSE
  if (viewport$x.min < min(x)) {
    x <- c(viewport$x.min, x)
    addViewportMin <- TRUE
  }

  addViewportMax <- FALSE
  if (viewport$x.max > max(x)) {
    x <- c(x, viewport$x.max)
    addViewportMax <- TRUE
  }
  
  if (is.null(myMethod)) {
    myMethod <- ceiling(as.numeric(max(x) - min(x)) / binWidth)
  }

  bins <- as.character(varrank::discretization(x, discretization.method = myMethod)$data.df)
  if (addViewportMin) {
    bins <- bins[x != viewport$x.min]
    x <- x[x != viewport$x.min]
  }
  if (addViewportMax) {
    bins <- bins[x != viewport$x.max]
  }

  return(bins)
}

bin.POSIXct <- function(x, binWidth = NULL, viewport) {
  if (is.null(binWidth)) {
    binWidth = findBinWidth(x, viewport)
  }

  addViewportMin <- FALSE
  if (viewport$min < min(x)) {
    x <- c(viewport$min, x)
    addViewportMin <- TRUE
  } else {
    x <- x[x >= viewport$min]
  }

  addViewportMax <- FALSE
  if (viewport$max > max(x)) {
    x <- c(x, viewport$max)
    addViewportMax <- TRUE
  } else {
    x <- x[x <= viewport$max]
  }

  bins <- as.Date(cut(x, breaks=binWidth))
  if (addViewportMin) {
    bins <- bins[x != viewport$min]
  }
  if (addViewportMax) {
    bins <- bins[x != viewport$max]
  }
  bins <- paste0(bins, " - ", lubridate::ceiling_date(bins, binWidth) -1)

  return(bins)
}

getMode <- function(x) {
   uniq <- unique(x)
   uniq[which.max(tabulate(match(x, uniq)))]
}

findBinStart <- function(x) {
  if (all(grepl(" - ",x))) {
    x <- unlist(lapply(strsplit(x, " - ", fixed=T), "[", 1))
  } else {
    x <- gsub("\\(|\\[", "", unlist(lapply(strsplit(as.character(x), ",", fixed=T), "[",1)))
  }

  return(x)
}

findBinEnd <- function(x) {
  if (all(grepl(" - ",x))) {
    x <- unlist(lapply(strsplit(x, " - ", fixed=T), "[", 2))
  } else {
    x <- gsub("\\)|\\]", "", unlist(lapply(strsplit(as.character(x), ",", fixed=T), "[", 2)))
  }

  return(x)
}

epitabToDT <- function(m, method) {
  dt <- data.table::as.data.table(m)
  dt$x.label <- rownames(m)
  dt <- transform(dt, "interval" = paste(lower, " - ", upper))
  dt$lower <- NULL
  dt$upper <- NULL
  dt$y.label <- list(names(dt)[c(1,3)])
  names(dt) <- c('cond1', 'proportion1', 'cond2', 'proportion2', method, 'p.value', 'x', 'interval', 'y.label')  
  dt[, y := lapply(transpose(.(cond1, cond2)), as.vector)]
  dt <- dt[, -c(1:4)]

  return(dt)
}

#' Odds Ratio
#'
#' This function calculates odds ratio, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'x' and 'y'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @return data.table with one row per group
#' @export
#' @importFrom epitools epitab
oddsRatio <- function(data) {
  m <- epitools::epitab(data$x, data$y, method = "oddsratio")$tab
  dt <- epitabToDT(m, 'oddsratio')

  return(dt)
}

#' Relative Risk
#'
#' This function calculates relative risk, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'x' and 'y'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @return data.table with one row per group
#' @export
relativeRisk <- function(data) {
  m <- epitools::epitab(data$x, data$y, method = "riskratio")$tab
  dt <- epitabToDT(m, 'relativerisk')

  return(dt)
}


bothRatios <- function(data) {
  mergeByCols <- c('p.value', 'x.label', 'y')

  or <- oddsRatio(data)
  names(or) <- c('oddsratio', 'p.value', 'x', 'or.interval', 'y')
  rr <- relativeRisk(data)
  names(rr) <- c('relativerisk', 'p.value', 'x', 'rr.interval', 'y')
  rr <- rr[, -(mergeByCols), with=FALSE]
  dt <- merge(or, rr, by=mergeByCols)

  return(dt)
}

chiSq <- function(data) {
  tbl <- table(data$x, data$y)
  dt <- as.data.frame.matrix(tbl)
  dt$y.label <- list(names(dt))
  dt$x.label <- rownames(dt)
  names(dt) <- c('cond1', 'cond2', 'y.label', 'x.label')
  dt <- data.table::as.data.table(dt)
  dt[, y := lapply(transpose(.(cond1, cond2)), as.vector)]
  dt <- dt[, -c(1:2)]
  chisq <- chisq.test(tbl)
  dt$chisq <- chisq$statistic
  dt$p.value <- chisq$p.value
  dt$degrees.freedom <- chisq$parameter

  return(dt)
}

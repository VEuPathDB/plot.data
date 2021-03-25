' Binning
#' 
#'
#' This function divides the range of ‘x’ into intervals and codes 
#' the values in ‘x’ according to which interval they fall
#' @param x Numeric or Date vector to bin
#' @param binWidth number to increment bin bounds by, or string for dates ex: 'month'
#' @return Character vector of coded values 
#' @export
#' @importFrom lubridate ceiling_date
#' @importFrom moments skewness
# @alias bin.numeric
# @alias bin.POSIXct
bin <- function(x, binWidth, viewport) UseMethod("bin")

bin.numeric <- function(x, binWidth = NULL, viewport) {
  xVP <- adjustToViewport(x, viewport)

  if (!is.null(binWidth)) {
    numBins <- binWidthToNumBins(xVP, binWidth)
  } else {
    numBins <- findNumBins(xVP)
  }

  bins <- cut(xVP, breaks=numBins)
  bins <- pruneViewportAdjustmentFromBins(bins, xVP, x, viewport)
  bins <- as.character(bins)

  return(bins)
}

#use stri_c where we paste dates bc it can be a bit faster w large vectors
#' @importFrom stringi stri_c
bin.Date <- function(x, binWidth = NULL, viewport) {
  xVP <- adjustToViewport(x, viewport)

  if (is.null(binWidth)) {
    binWidth = findBinWidth(xVP)
  }

  bins <- as.Date(cut(xVP, breaks=binWidth))
  bins <- pruneViewportAdjustmentFromBins(bins, xVP, x, viewport)
  bins <- stringi::stri_c(bins, " - ", lubridate::ceiling_date(bins, binWidth) -1)

  return(bins)
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
findBinWidth <- function(x) UseMethod("findBinWidth")

findBinWidth.numeric <- function(x) {
  numBins <- findNumBins(x)
  binWidth <- numBinsToBinWidth(x, numBins)

  return(binWidth)
}

findBinWidth.Date <- function(x) {
  dateMap <- data.table('date' = x, 'numeric' = as.numeric(x))
  binWidth <- findBinWidth(dateMap$numeric)

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

#' @importFrom grDevices nclass.FD
#' @importFrom grDevices nclass.Sturges
findNumBins <- function(x) {
  numBins <- NULL

  if (length(x) < 200) {
    numBins <- grDevices::nclass.FD(x)
  }
  skewness <- moments::skewness(x)
  if (abs(skewness) > .5) {
    abs <- abs(skewness)
    n <- length(x)
    se <- sqrt(6*(n-2)/((n+1)*(n+3)))
    ke <- log2(1+abs/se)
    numBins <- ceiling(nclass.Sturges(x)+ke)
  }
  if (is.null(numBins)) {
    numBins <- grDevices::nclass.Sturges(x)
  }

  return(numBins)
}

#NOTE: bc of the ceiling, cant depend on each other
binWidthToNumBins <- function(x, binWidth) {
  ceiling(diff(range(x))/binWidth)
}

numBinsToBinWidth <- function(x, numBins) {
  diff(range(x))/numBins
}

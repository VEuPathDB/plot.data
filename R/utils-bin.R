#' Binning
#' 
#'
#' This function divides the range of ‘x’ into intervals and codes 
#' the values in ‘x’ according to which interval they fall
#' @param x Numeric or Date vector to bin
#' @param binWidth number to increment bin bounds by, or string for dates ex: 'month'
#' @param viewport List of min and max values to consider as the range of data
#' @param stringsAsFactors logical indicating whether a factor or character vector should be returned
#' @return Character vector (or factor) of coded values 
#' @export
#' @importFrom lubridate ceiling_date
#' @importFrom moments skewness
# @alias bin.numeric
# @alias bin.POSIXct
bin <- function(x, binWidth, viewport, stringsAsFactors) UseMethod("bin")

#' @export
bin.numeric <- function(x, binWidth = NULL, viewport, stringsAsFactors = c(FALSE, TRUE)) {
  stringsAsFactors <- veupathUtils::matchArg(stringsAsFactors)

  if (!length(x)) { return(character(0)) }
  if (all(is.na(x))) { return(character(0)) }

  xVP <- adjustToViewport(x, viewport)

  if (!is.null(binWidth)) {
    if (binWidth == 0) {
      bins <- xVP
    } else {
      bins <- cut_width(xVP, binWidth, boundary = min(xVP))
    }
  } else {
    numBins <- findNumBins(xVP)
    bins <- cut_interval(xVP, numBins)
  }

  bins <- pruneViewportAdjustmentFromBins(bins, xVP, x, viewport)
  if (!stringsAsFactors) bins <- as.character(bins)

  return(bins)
}

#use stri_c where we paste dates bc it can be a bit faster w large vectors
#' @importFrom stringi stri_c
#' @importFrom lubridate days
#' @importFrom lubridate weeks
#' @importFrom lubridate years
#' @export
bin.Date <- function(x, binWidth = NULL, viewport, stringsAsFactors = c(FALSE, TRUE)) {
  stringsAsFactors <- veupathUtils::matchArg(stringsAsFactors)

  if (!length(x)) { return(character(0)) }
  if (all(is.na(x))) { return(character(0)) }

  xVP <- adjustToViewport(x, viewport)

  if (is.null(binWidth)) {
    binWidth = findBinWidth(xVP)
  }

  if (binWidth == 0) {
    bins <- xVP
  } else {
    binStart <- cut(xVP, breaks=binWidth)
    binLevelsStart <- levels(binStart)
    binStart <- as.Date(binStart)
    binStart <- pruneViewportAdjustmentFromBins(binStart, xVP, x, viewport)

    if (grepl("^[[:digit:]].", binWidth) & gsub("[^0-9.-]", "", binWidth) != '1') {
      #works bc we assume a single space between the binWidth and unit
      unit <- veupathUtils::trim(gsub("^[[:digit:]]*", "", binWidth))
      numericBinWidth <- as.numeric(gsub("[^0-9.-]", "", binWidth))
      if (unit %in% c('day','days')) {
        binEnd <- as.Date(binStart + lubridate::days(numericBinWidth))
        binLevelsEnd <- as.Date(binLevelsStart + lubridate::days(numericBinWidth))
      } else if (unit %in% c('week', 'weeks')) {
        binEnd <- as.Date(binStart + lubridate::weeks(numericBinWidth))
        binLevelsEnd <- as.Date(binLevelsStart + lubridate::weeks(numericBinWidth))
      } else if (unit %in% c('month', 'months')) {
        binEnd <- as.Date(binStart + months(numericBinWidth))
        binLevelsEnd <- as.Date(binLevelsStart + months(numericBinWidth))
      } else if (unit %in% c('year', 'years')) {
        binEnd <- as.Date(binStart + lubridate::years(numericBinWidth))
        binLevelsEnd <- as.Date(binLevelsStart + lubridate::years(numericBinWidth))
      } else {
        stop("Unrecognized units for binning date histogram.")
      }    
    } else {
      #for some reason week doesnt do whats expected..
      if (binWidth %in% c('week', 'weeks', '1 week')) {
        binEnd <- as.Date(binStart + lubridate::days(7))
        binLevelsEnd <- as.Date(binLevelsStart + lubridate::days(7))
      } else {
        binEnd <- lubridate::ceiling_date(as.Date(binStart), binWidth)
        binLevelsEnd <- lubridate::ceiling_date(as.Date(binLevelsStart), binWidth)
      }
    }

    bins <- stringi::stri_c(binStart, " - ", binEnd)
    if (stringsAsFactors) {
      binLevels <- stringi::stri_c(binLevelsStart, " - ", binLevelsEnd)
      bins <- factor(bins, levels=binLevels)
    }  

    return(bins)    
  }

  

  return(bins)
}

#' Calculate Bin Width
#' 
#' This function determines the ideal bin width based on the range,
#' sample size and distribution of values.
#' @param x Numeric or Date vector
#' @param na.rm boolean indicating if missing values should be removed
#' @return Numeric or character bin width
#' @export
# @alias findBinWidth.numeric
# @alias findBinWidth.POSIXct
# @alias findBinWidth.logical
# @alias findBinWidth.NULL
findBinWidth <- function(x, na.rm = c(FALSE, TRUE)) UseMethod("findBinWidth")

#' @export
findBinWidth.NULL <- function(x, na.rm = c(FALSE, TRUE)) { NULL }

#' @export
findBinWidth.logical <- function(x, na.rm = c(FALSE, TRUE)) { NA }

#' @export
findBinWidth.numeric <- function(x, na.rm = c(FALSE, TRUE)) {
  na.rm <- veupathUtils::matchArg(na.rm)
  if (na.rm) {
    x <- x[complete.cases(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  if (all(x %% 1 == 0)) {
    isInteger <- TRUE
    if (data.table::uniqueN(x) == 1) { return(1) }
  } else {
    isInteger <- FALSE
    if (data.table::uniqueN(x) == 1) { return(0) }
  } 
  numBins <- findNumBins(x)
  if (is.null(numBins)) { return(NULL) }
  binWidth <- numBinsToBinWidth(x, numBins)
  
  if (isInteger) {
    # binWidth should also be an integer
    avgDigits <- 0
    binWidth <- veupathUtils::nonZeroRound(binWidth, avgDigits)
    if (binWidth < 1) { binWidth <- 1}
  } else {
    # binWidth can be any float
    avgDigits <- floor(mean(stringi::stri_count_regex(as.character(x), "[[:digit:]]")))
    binWidth <- veupathUtils::nonZeroRound(binWidth, avgDigits)
  }

  return(binWidth)
}

#' @export
findBinWidth.Date <- function(x, na.rm = c(FALSE, TRUE)) {
  na.rm <- veupathUtils::matchArg(na.rm)
  if (na.rm) {
    x <- x[complete.cases(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  if (data.table::uniqueN(x) == 1) {
    return('day')
  }

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
  if (!length(x)) { return(NULL) }
  numBins <- NULL

  if (length(x) > 200) {
    numBins <- grDevices::nclass.FD(x)
  }
  skewness <- moments::skewness(x)
  if (abs(skewness) > .5) {
    abs <- abs(skewness)
    n <- length(x)
    se <- sqrt(6*(n-2)/((n+1)*(n+3)))
    ke <- log2(1+abs/se)
    numBins <- ceiling(grDevices::nclass.Sturges(x)+ke)
  }
  if (is.null(numBins)) {
    numBins <- grDevices::nclass.Sturges(x)
  }

  return(numBins)
}

#NOTE: bc of the ceiling, cant depend on each other
#' @export
binWidthToNumBins <- function(x, binWidth) {
  ceiling(diff(range(x))/binWidth)
}

#' @export
numBinsToBinWidth <- function(x, numBins) UseMethod("numBinsToBinWidth")

#' @export
numBinsToBinWidth.default <- function(x, numBins) {
  if (data.table::uniqueN(x) <= numBins) { return(0) }

  numDigits <- ifelse( avgDigits(x) > 6, 4, avgDigits(x) - 1)
  binWidth <- veupathUtils::nonZeroRound(diff(range(x)), numDigits)/numBins
  if (all(x %% 1 == 0)) {
    binWidth <- ceiling(binWidth)
    if (numBins == 1) {
      binWidth <- binWidth + 1
    }
  }
  
  return(binWidth)
}

#' @export
numBinsToBinWidth.Date <- function(x, numBins) {
  if (data.table::uniqueN(x) <= numBins) { return(0) }

  paste(ceiling(as.numeric(diff(range(x))/numBins)), 'days')
}

#' @export
findBinSliderValues <- function(x, xType, binWidth, binReportValue, maxNumBins) UseMethod("findBinSliderValues")

#' @export
findBinSliderValues.numeric <- function(x, xType, binWidth = NULL, binReportValue = 'binWidth', maxNumBins = 1000) {
  if (binReportValue == 'numBins') {
    return(list('min'=jsonlite::unbox(2), 'max'=jsonlite::unbox(1000), 'step'=jsonlite::unbox(1)))
  }

  binSliderMax <- as.numeric((max(x) - min(x)) / 2)
  binSliderMin <- as.numeric((max(x) - min(x)) / maxNumBins)
  avgDigits <- floor(mean(stringi::stri_count_regex(as.character(x), "[[:digit:]]")))
  binSliderMax <- veupathUtils::nonZeroRound(binSliderMax, avgDigits)
  binSliderMin <- veupathUtils::nonZeroRound(binSliderMin, avgDigits)
  binSliderStep <- veupathUtils::nonZeroRound(((binSliderMax - binSliderMin) / 1000), avgDigits)
  binSliderMin <- ifelse(xType == 'INTEGER' && binSliderMin %% 1 != 0, ceiling(binSliderMin), binSliderMin)
  binSliderStep <- ifelse(xType == 'INTEGER' && binSliderStep %% 1 != 0, ceiling(binSliderStep), binSliderStep)
  binSliderMax <- ifelse(xType == 'INTEGER' && binSliderMax %% 1 != 0, ceiling(binSliderMax), binSliderMax)
  ## these cases should be rare. theyd have to have a single value for x.
  binSliderMin <- ifelse(binSliderMin == 0, .1, binSliderMin)
  binSliderStep <- ifelse(binSliderStep == 0, binSliderMin, binSliderStep)

  return(list('min'=jsonlite::unbox(binSliderMin), 'max'=jsonlite::unbox(binSliderMax), 'step'=jsonlite::unbox(binSliderStep)))
}

#' @export
findBinSliderValues.Date <- function(x, xType, binWidth = NULL, binReportValue = 'binWidth', maxNumBins = 1000) {
  if (binReportValue == 'numBins') {
    list('min'=jsonlite::unbox(2), 'max'=jsonlite::unbox(maxNumBins), 'step'=jsonlite::unbox(1))
  }
  
  binSliderMax <- as.numeric((max(x) - min(x)) / 2)
  binSliderMin <- as.numeric((max(x) - min(x)) / maxNumBins)
  if (!is.null(binWidth) & binWidth != 0) {
    unit <- veupathUtils::trim(gsub("^[[:digit:]].", "", binWidth))
  } else {
    unit <- 'day'
  }
  
  if (unit %in% c('day', 'days')) {
    binSliderMin <- floor(binSliderMin)
    binSliderMax <- ceiling(binSliderMax)
  } else if (unit %in% c('week', 'weeks')) {
    numWeeks <- floor(as.numeric(difftime(max(x), min(x), units='weeks'))) 
    binSliderMin <- floor(numWeeks/maxNumBins)
    binSliderMax <- ceiling(numWeeks/2)
  } else if (unit %in% c('month', 'months')) {
    numMonths <- uniqueN(zoo::as.yearmon(x))
    binSliderMin <- floor(numMonths/maxNumBins)
    binSliderMax <- ceiling(numMonths/2) 
  } else if (unit %in% c('year', 'years')) {
    numYears <- uniqueN(veupathUtils::strSplit(as.character(x), '-', 3))
    binSliderMin <- floor(numYears/maxNumBins)
    binSliderMax <- ceiling(numYears/2)
  } else {
    stop("Unrecognized unit for date bins.")
  }
  binSliderMin <- ifelse(binSliderMin == 0, 1, binSliderMin)
  binSliderStep <- 1

  return(list('min'=jsonlite::unbox(binSliderMin), 'max'=jsonlite::unbox(binSliderMax), 'step'=jsonlite::unbox(binSliderStep)))
}

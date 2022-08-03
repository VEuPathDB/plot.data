# these functions largely borrowed from ggplot2.
# ive copied rather than imported them for increased control
# this was decided after finding a bug or two
# if we decide theyre generally useful, consider renaming + exporting
# curious if its worth a PR to ggplot2

cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, breaks(x, "width", n, length), include.lowest = TRUE, ...)
}

cut_number <- function(x, n = NULL, ...) {
  brk <- breaks(x, "n", n)
  if (anyDuplicated(brk))
    stop(glue("Insufficient data values to produce {n} bins."))
  cut(x, brk , include.lowest = TRUE, ...)
}

cut_width <- function(x, width, center = NULL, boundary = NULL, closed = c("right", "left"), ...) {
  x <- as.numeric(x)
  width <- as.numeric(width)

  closed <- veupathUtils::matchArg(closed)

  x_range <- range(x, na.rm = TRUE, finite = TRUE)
  if (length(x_range) == 0) {
    return(x)
  }

  # Determine boundary
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }
  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }
  boundary <- as.numeric(boundary)

  # Determine bins
  min_x <- find_origin(x_range, width, boundary)
  max_x <- max(x, na.rm = TRUE)

  breaks <- c(seq(min_x, max_x, width))
  # Round breaks *before* they go into the cut function. This way the data (not rounded)
  # will be correctly divided into the rounded bins
  if (all(x %% 1 == 0)) {
    # if all integers, use default formatting (6 significant digits)
    requiredDigits <- -1
  } else {
    # Determine the appropriate number of digits to round the bin start/ends to.
    # Start with the number of digits in the binWidth and keep increasing until the breaks are unique.
    requiredDigits <- stringi::stri_count_regex(as.character(width), "[[:digit:]]")
    rawBreaks <- breaks
    repeat {
      requiredDigits <- requiredDigits + 1
      breaks <- as.numeric(formatC(0 + rawBreaks, digits = requiredDigits, width = 1L))
      if (anyDuplicated(breaks) == 0 || requiredDigits > 100) { # safety escape condition
        break
      }
    }
  }

  # If now, after rounding, either of the start and end bin breaks fall INSIDE the range of the data,
  # add extra bins to one or both ends
  redoBreaks <- FALSE
  startBin <- min_x
  if (min_x < min(breaks)) {
    startBin <- min_x - (1 - 1e-08) * width
    # print("ADDING STARTBIN") # gets triggered quite a lot during test-histogram.R tests!
    redoBreaks <- TRUE
  }
  endBin <- max_x
  if (max_x > max(breaks)) {
    endBin <- max_x + (1 - 1e-08) * width
    # print("ADDING ENDBIN") # gets triggered quite a lot during test-histogram.R tests!
    redoBreaks <- TRUE
  }
  if (redoBreaks) {
    breaks <- c(seq(startBin, endBin, width))
    breaks <- as.numeric(formatC(0 + breaks, digits = requiredDigits, width = 1L))
  }
  cut(x, breaks, include.lowest = TRUE, right = (closed == "right"), dig.lab = requiredDigits, ...)
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
}

breaks <- function(x, equal, nbins = NULL, binwidth = NULL) {
  equal <- veupathUtils::matchArg(equal, c("numbers", "width"))
  if ((!is.null(nbins) && !is.null(binwidth)) || (is.null(nbins) && is.null(binwidth))) {
    stop("Specify exactly one of n and width")
  }

  rng <- range(x, na.rm = TRUE, finite = TRUE)
  if (equal == "width") {
    if (!is.null(binwidth)) {
      scales::fullseq(rng, binwidth)
    } else {
      seq(rng[1], rng[2], length.out = nbins + 1)
    }
  } else {
    if (!is.null(binwidth)) {
      probs <- seq(0, 1, by = binwidth)
    } else {
      probs <- seq(0, 1, length.out = nbins + 1)
    }
    stats::quantile(x, probs, na.rm = TRUE)
  }

}

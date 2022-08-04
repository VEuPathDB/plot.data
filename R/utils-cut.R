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
    # if all integers, use default formatting in cut() (6 significant digits)
    # But the binWidth might not be an integer? Why don't we round the breaks?
    # Should that be all(breaks %% 1 == 0) ?? (All tests pass if we do that.)
    requiredDigits <- -1
  } else {
    # Determine the appropriate number of digits to round the bin start/ends to.
    # Start at 4 and work up!
    requiredDigits <- 4
    rawBreaks <- breaks
    repeat {
      breaks <- as.numeric(formatC(0 + rawBreaks, digits = requiredDigits, width = 1L))
      if (anyDuplicated(breaks) == 0 || requiredDigits > 100) { # safety escape condition
        break
      }
      requiredDigits <- requiredDigits + 1
    }
  }

  # If, after rounding, either of the start and end bin breaks fall INSIDE the range of the data,
  # add extra breaks to one or both ends

  # but first catch the case where there is only one break
  if (length(breaks) == 1) {
    breaks = c(breaks, tail(breaks,1) + width)
    #I've left these debugging statements in temporarily in case you're curious
    #print("ONLY ONE BREAK")
  }

  # note that we work with the already-rounded breaks here
  # (we used to reround them again after adding new breaks)
  if (min_x < head(breaks,1)) {
    breaks = c(head(breaks,1) - diff(head(breaks,2)), breaks)
    #print("EXTRA START BIN")
  }
  # sometimes, due to rounding, one extra bin isn't enough
  # though I think this happens more with the end bins
  if (min_x < head(breaks,1)) {
    breaks = c(head(breaks,1) - diff(head(breaks,2)), breaks)
    #print("EXTRA EXTRA START BIN")
  }

  if (max_x > tail(breaks,1)) {
    breaks = c(breaks, tail(breaks,1) + diff(tail(breaks,2)))
    #print("EXTRA END BIN")
  }
  # sometimes, due to rounding, one extra bin isn't enough
  if (max_x > tail(breaks,1)) {
    breaks = c(breaks, tail(breaks,1) + diff(tail(breaks,2)))
    #print("EXTRA EXTRA END BIN")
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

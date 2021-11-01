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
  # Small correction factor so that we don't get an extra bin when, for
  # example, origin = 0, max(x) = 20, width = 10.
  max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width

  breaks <- c(seq(min_x, max_x, width), max_x)
  cut(x, breaks, include.lowest = TRUE, right = (closed == "right"), ...)
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
      fullseq(rng, binwidth)
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

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
contingencyDT <- function(data) {
  dt <- as.data.frame.matrix(table(data$x, data$y))
  dt$label <- rownames(dt)

  return(as.data.table(dt))
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
makePanels <- function(data, facet1Name = NULL, facet2Name = NULL) {
  if (!is.null(facet1) && !is.null(facet2)) {
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
  if (x == "") { x <- "." }

  return(x)
}

emptyStringToNull <- function(x) {
  if (x == "") { x <- NULL }

  return(x)
}

#TODO consider if empty grouping vars are NULL or go to '.'
getAggStr <- function(numericVars, groupingVars) {
  numericString <- emptyStringToPoint(paste(numericVars, collapse= " + "))
  groupingString <- emptyStringToNull(paste(groupingVars, collapse=" + "))
  aggStr <- paste(c(numericString, groupingString), collapse=" ~ ")

  return(aggStr)
}

#' Fences
#'
#' This function returns the lower and upper fences for a numeric
#' vector. The lower fence is calculated as `q1 - 1.5*IQR` 
#' and the upper as `q3 + 1.5*IQR`.
#' @param x Numeric vector to calculate fences for
#' @return Numeric vector of length 2: c(lowerfence, upperfence)
#' @export
fences <- function(x) {
  summary <- stats::quantile(x)
  iqr <- summary[4] - summary[2]
  lowerfence <- summary[2] - (1.5*iqr)
  upperfence <- summary[4] + (1.5*iqr)

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

#' Binning
#'
#' This function divides the range of ‘x’ into intervals and codes 
#' the values in ‘x’ according to which interval they fall
#' @param x Numeric vector to bin
#' @param binWidth number to increment bin bounds by
#' @return Character vector of coded values 
#' @export
bin <- function(x, binWidth) {
  summary <- stats::quantile(x)
  bounds <- c(summary[1], summary[5])
  bounds <- sign(bounds) * ceiling(abs(bounds) * 100) / 100
  breaks <- seq(bounds[1], bounds[2], binWidth)
  breaks <- c(breaks, (breaks[length(breaks)] + binWidth))

  return(as.character(cut(x, breaks=breaks)))
}

epitabToDT <- function(m, method) {
  dt <- data.table::as.data.table(m)
  dt$group <- rownames(m)
  dt$x <- list(names(dt)[c(1,3)])
  dt <- transform(dt, "interval" = paste(lower, " - ", upper))
  dt$lower <- NULL
  dt$upper <- NULL
  names(dt) <- c('cond1', 'proportion1', 'cond2', 'proportion2', method, 'pvalue', 'group', 'x', 'interval')
  dt$y <- lapply(dt$group, FUN = function(x){c(dt$cond1[dt$group == x], dt$cond2[dt$group == x])})
  dt$proportions <- lapply(dt$group, FUN = function(x){c(dt$proportion1[dt$group == x], dt$proportion2[dt$group == x])})
  dt$cond1 <- NULL
  dt$cond2 <- NULL
  dt$proportion1 <- NULL
  dt$proportion2 <- NULL

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
  m <- epitools:epitab(data$x, data$y, method = "riskratio")$tab
  dt <- epitabToDT(m, 'relativerisk')

  return(dt)
}



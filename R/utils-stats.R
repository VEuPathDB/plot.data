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
  lowerfence <- min(x[x >= lowerfence])
  upperfence <- summary[4] + (1.5*iqr)
  upperfence <- max(x[x <= upperfence])

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
  dt <- data.table::data.table("densityX" = c(curve$x), "densityY" = c(curve$y))

  return(dt)
}

# Prediction data frame
# Get predictions with standard errors into data frame
#
# @keyword internal
# @alias predictdf.gam
# @alias predictdf.loess
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

predictdf.default <- function(model, xseq, se = FALSE, level = .95) {
  pred <- stats::predict(model, newdata = new_data_frame(list(x = xseq)), se.fit = se,
    level = level, interval = if (se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "ymin", "ymax")
    base::data.frame(x = xseq, fit, se = pred$se.fit)
  } else {
    base::data.frame(x = xseq, y = as.vector(pred))
  }
}

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

#' Best Fit Line from Linear Regression
#'
#' This function returns for the range of x values the line of best
#' fit and corresponding R-squared measure.
#' Calculation is performed by the (currently undocumented)
#' `predictdf()` generic and its methods. `stats::lm` is used to build
#' the regression model w formula `y~x`.
#' @param dt data.frame with at least two columns: x) representing the independent variable and y) representing the dependent variable
#' @return data.table with the follwing columns x, y (best fit line) and r2 (R-squared)

#' @export
bestFitLine <- function(dt, collapse = TRUE) {
  xseq <- sort(unique(dt$x))
  linearModel <- stats::lm(y ~ x, dt)

  bestFitLine <- data.table::as.data.table(predictdf(linearModel, xseq))
  if (collapse) {
    bestFitLine <- bestFitLine[, lapply(.SD, list)]
  }
  data.table::setnames(bestFitLine, c('bestFitLineX', 'bestFitLineY'))
  # if not collapsed, this col will be repetititve
  # only good way around this is a dedicated class
  # which probably not worth while considering how this fxn is used
  bestFitLine$r2 <- summary(linearModel)$r.squared

  return(bestFitLine)
}

#' Smoothed Conditional Mean
#'
#' This function returns for the range of x values the predicted mean,
#' 95% confidence interval and standard error.
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
smoothedMean <- function(dt, method, collapse = TRUE) {
  xseq <- sort(unique(dt$x))

  if (method == 'loess') {
    smoothed <- stats::loess(y ~ x, dt)
  } else if (method == 'gam') {
    smoothed <- mgcv::gam(y ~ s(x, bs = "cs"), data = dt, method = "REML")
  } else {
    stop('Unrecognized smoothing method.')
  }

  smoothed <- data.table::as.data.table(predictdf(smoothed, xseq))

  if (collapse) {
    dt <- data.table::data.table("smoothedMeanX" = list(smoothed$x), "smoothedMeanY" = list(smoothed$y), "smoothedMeanSE" = list(smoothed$se))
  } else {
    dt <- data.table::data.table("smoothedMeanX" = smoothed$x, "smoothedMeanY" = smoothed$y, "smoothedMeanSE" = smoothed$se)
  }

  return(dt)
}

epitabToDT <- function(m, method) {
  dt <- data.table::as.data.table(m)
  dt$x.label <- rownames(m)
  dt <- transform(dt, "interval" = paste(lower, " - ", upper))
  dt$lower <- NULL
  dt$upper <- NULL
  dt$y.label <- list(names(dt)[c(1,3)])
  data.table::setnames(dt, c('cond1', 'proportion1', 'cond2', 'proportion2', method, 'p.value', 'x', 'interval', 'y.label'))
  dt[, y := lapply(transpose(.(cond1, cond2)), as.vector)]
  dt <- dt[, -c(1:4)]

  return(dt)
}

#' Odds Ratio
#'
#' This function calculates odds ratio, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'x' and 'y'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @param collapse boolean indicating whether or not to collapse the data.table to have only one row per group. If true, values per group will be represented as lists within the data.table.
#' @return data.table, optionally with only one row per group
#' @export
#' @importFrom epitools epitab
oddsRatio <- function(data, collapse = TRUE) {
  m <- epitools::epitab(data$x, data$y, method = "oddsratio")$tab
  dt <- epitabToDT(m, 'oddsratio')

  if (collapse) {
    dt <- collapseByGroup(dt)
  }

  return(dt)
}

#' Relative Risk
#'
#' This function calculates relative risk, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'x' and 'y'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @param collapse boolean indicating whether or not to collapse the data.table to have only one row per group. If true, values per group will be represented as lists within the data.table.
#' @return data.table, optionally with only one row per group
#' @export
relativeRisk <- function(data, collapse = TRUE) {
  m <- epitools::epitab(data$x, data$y, method = "riskratio")$tab
  dt <- epitabToDT(m, 'relativerisk')

  if (collapse) {
    dt <- collapseByGroup(dt)
  }

  return(dt)
}

bothRatios <- function(data, collapse = TRUE) {
  mergeByCols <- c('pvalue', 'yLabel', 'xLabel', 'value')

  or <- oddsRatio(data, collapse)
  data.table::setnames(or, c('oddsratio', 'pvalue', 'xLabel', 'orInterval', 'yLabel', 'value'))
  rr <- relativeRisk(data, collapse)
  data.table::setnames(rr, c('relativerisk', 'pvalue', 'xLabel', 'rrInterval', 'yLabel', 'value'))
  if (collapse) { 
    if (!identical(or$yLabel, rr$yLabel) |
        !identical(or$xLabel, rr$xLabel) |
        !identical(or$pvalue, rr$pvalue) |
        !identical(or$value, rr$value)) {
      stop('cannot merge odds ratio and relative risk data!')
    }
    rr <- rr[, -mergeByCols, with=FALSE]
    dt <- cbind(or, rr)
  } else {
    dt <- merge(or, rr, by = mergeByCols)
  }

  return(dt)
}

chiSq <- function(data, collapse = TRUE) {
  tbl <- table(data$x, data$y)
  dt <- as.data.frame.matrix(tbl)
  data.table::setDT(dt)
  dt[, value := lapply(transpose(.SD), as.vector)]
  dt$yLabel <- list(names(dt)[names(dt) != 'value'])
  dt$xLabel <- rownames(tbl)
  dt <- dt[, c('value','yLabel','xLabel'), with=FALSE]
  if (collapse) {
    dt <-  collapseByGroup(dt)
  }
  chisq <- chisq.test(tbl)
  dt$chisq <- chisq$statistic
  dt$pvalue <- chisq$p.value
  dt$degreesFreedom <- chisq$parameter

  return(dt)
}

getMode <- function(x) {
   uniq <- unique(x)
   uniq[which.max(tabulate(match(x, uniq)))]
}

getR2 <- function(model) UseMethod("getR2")

#default assumes model class lm for now, can revisit as needed
getR2.default <- function(model) {
  summary(model)$r.squared
}

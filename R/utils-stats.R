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
#' @return data.table with two columns: independent) the coordinates of the points where the density is estimated and dependent) the estimated density values. These will be non-negative, but can be zero. 
#' @export
#' @import data.table
densityCurve <- function(x) {
  curve <- stats::density(x)

  return(data.table::data.table("independent" = c(curve$x), "dependent" = c(curve$y)))
}

# Prediction data frame
# Get predictions with standard errors into data frame
#
# @keyword internal
# @alias predictdf.gam
# @alias predictdf.loess
predictdf <- function(model, independentSeq, se, level) UseMethod("predictdf")

predictdf.loess <- function(model, independentSeq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(independent = independentSeq), se = se)

  if (se) {
    dependentHat = pred$fit
    ci <- pred$se.fit * stats::qt(level / 2 + .5, pred$df)
    dependentHatMin = dependentHat - ci
    dependentHatMax = dependentHat + ci
    base::data.frame(independent = independentSeq, dependent = dependentHat, dependentHatMin, dependentHatMax, se = pred$se.fit)
  } else {
    base::data.frame(independent = independentSeq, dependent = as.vector(pred))
  }
}

predictdf.gam <- function(model, independentSeq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(independent = independentSeq), se.fit = se,
    type = "link")

  if (se) {
    std <- stats::qnorm(level / 2 + 0.5)
    base::data.frame(
      independent = independentSeq,
      dependent = model$family$linkinv(as.vector(pred$fit)),
      ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se.fit)),
      ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se.fit)),
      se = as.vector(pred$se.fit)
    )
  } else {
    base::data.frame(independent = independentSeq, dependent = model$family$linkinv(as.vector(pred)))
  }
}

#' Smoothed Conditional Mean
#'
#' This function returns for every independent value the predicted mean, 95% confidence interval and standard error.
#' Calculation is performed by the (currently undocumented)
#' `predictdf()` generic and its methods. `loess`, uses a t-based 
#' approximation, and for `gam` the normal confidence interval is 
#' constructed on the link scale and then back-transformed to 
#' the response scale.
#' @param dt data.frame with at least two columns: independent) representing the independent variable and dependent) representing the dependent variable
#' @param method Character string one of 'loess' or 'gam'
#' @return data.table with the follwing columns independent, dependent (predicted mean), ymin, ymax (lower and upper bound of confidence interval), se (standard error) 

#' @export
#' @importFrom mgcv gam
smoothedMean <- function(dt, method) {
  independentSeq <- sort(unique(dt$independent))

  if (method == 'loess') {
    smoothed <- stats::loess(dependent ~ independent, dt)
  } else if (method == 'gam') {
    smoothed <- mgcv::gam(dependent ~ s(independent, bs = "cs"), data = dt, method = "REML")
  } else {
    stop('Unrecognized smoothing method.')
  }

  smoothed <- data.table::as.data.table(predictdf(smoothed, independentSeq))

  return(data.table::data.table("independent" = list(smoothed$independent), "dependent" = list(smoothed$dependent), "dependentHatMin" = list(smoothed$dependentHatMin), "dependentHatMax" = list(smoothed$dependentHatMax), "se" = list(smoothed$se))) #### predictedMin, predictedMax?
}

epitabToDT <- function(m, method) {
  dt <- data.table::as.data.table(m)
  dt$independent.label <- rownames(m)
  dt <- transform(dt, "interval" = paste(lower, " - ", upper))
  dt$lower <- NULL
  dt$upper <- NULL
  dt$dependent.label <- list(names(dt)[c(1,3)])
  names(dt) <- c('cond1', 'proportion1', 'cond2', 'proportion2', method, 'p.value', 'independent', 'interval', 'dependent.label')
  dt[, dependent := lapply(transpose(.(cond1, cond2)), as.vector)]
  dt <- dt[, -c(1:4)]

  return(dt)
}

#' Odds Ratio
#'
#' This function calculates odds ratio, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'independent' and 'dependent'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @return data.table with one row per group
#' @export
#' @importFrom epitools epitab
oddsRatio <- function(data) {
  m <- epitools::epitab(data$independent, data$dependent, method = "oddsratio")$tab
  dt <- epitabToDT(m, 'oddsratio')
  dt <- noStatsFacet(dt)

  return(dt)
}

#' Relative Risk
#'
#' This function calculates relative risk, confidence intervals and p-values for epidemiologic data 
#' @param data A data.table with two columns 'independent' and 'dependent'. The two will be combined into a table. The first is the independent variable and can have any number of values. The second is the dependent variable, and should have two unique values.
#' @return data.table with one row per group
#' @export
relativeRisk <- function(data) {
  m <- epitools::epitab(data$independent, data$dependent, method = "riskratio")$tab
  dt <- epitabToDT(m, 'relativerisk')
  dt <- noStatsFacet(dt)

  return(dt)
}

bothRatios <- function(data) {
  mergeByCols <- c('p.value', 'dependent.label', 'independent.label', 'dependent')

  or <- oddsRatio(data)
  names(or) <- c('oddsratio', 'p.value', 'independent.label', 'or.interval', 'dependent.label', 'dependent')
  rr <- relativeRisk(data)
  names(rr) <- c('relativerisk', 'p.value', 'independent.label', 'rr.interval', 'dependent.label', 'dependent')
  if (!identical(or$dependent.label, rr$dependent.label) |
      !identical(or$independent, rr$independent) |
      !identical(or$p.value, rr$p.value) |
      !identical(or$dependent, rr$dependent)) {
    stop('cannot merge odds ratio and relative risk data!')
  }
  rr <- rr[, -mergeByCols, with=FALSE]
  dt <- cbind(or, rr)

  return(dt)
}

chiSq <- function(data) {
  tbl <- table(data$independent, data$dependent)
  dt <- as.data.frame.matrix(tbl)
  dt <- data.table::as.data.table(dt)
  dt[, dependent := lapply(transpose(.SD), as.vector)]
  dt$dependent.label <- list(names(dt)[names(dt) != 'dependent'])
  dt$independent.label <- rownames(tbl)
  dt <- dt[, c('dependent','dependent.label','independent.label'), with=FALSE]
  dt <- noStatsFacet(dt)
  chisq <- chisq.test(tbl)
  dt$chisq <- chisq$statistic
  dt$p.value <- chisq$p.value
  dt$degrees.freedom <- chisq$parameter

  return(dt)
}

getMode <- function(x) {
   uniq <- unique(x)
   uniq[which.max(tabulate(match(x, uniq)))]
}

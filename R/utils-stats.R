formatPValue <- function(pvalue) {
  if (pvalue < 0.0001) return("<0.0001")
  return(as.character(signif(pvalue, 2)))
}

#' @importFrom stats qbeta
zexact <- function(numerator, denominator, conf.level){
  # Exact binomial confidence limits from function binom::binom.confint. 
  alpha <- 1 - conf.level
  alpha2 <- 0.5 * alpha
  
  p <- numerator / denominator
  a1 <- numerator == 0
  a2 <- numerator == denominator
  lb <- ub <- numerator
  lb[a1] <- 1
  ub[a2] <- denominator[a2] - 1
  low <- 1 - suppressWarnings(stats::qbeta(1 - alpha2, denominator + 1 - numerator, lb))
  upp <- 1 - suppressWarnings(stats::qbeta(alpha2, denominator - ub, numerator + 1))
  
  if (!is.na(a1)) if (any(a1)) low[a1] <- rep(0, sum(a1))
  
  if (!is.na(a2)) if (any(a2)) upp[a2] <- rep(1, sum(a2))
  
  return(data.frame(est = signif(p, 2), lower = signif(low, 2), upper = signif(upp, 2)))
}


medianCI <- function(x, conf.level=.95) {
  x <- x[order(x)]
  n <- length(x)
  z <- qnorm(p=(1-conf.level)/2, lower.tail=FALSE)
  j <- ceiling(.5*n - z*sqrt(n*.25))
  k <- ceiling(.5*n + z*sqrt(n*.25))

  if (j < 1 || k > length(x)) {
    ci <- list('lowerBound'=jsonlite::unbox(NA), 'upperBound'=jsonlite::unbox(NA), 'error'=jsonlite::unbox("Failed to determine confidence interval for median."))
  } else {
    ci <- list('lowerBound'=jsonlite::unbox(x[j]), 'upperBound'=jsonlite::unbox(x[k]), 'error'=jsonlite::unbox(""))
  }

  return(list(ci))
}

meanCI <- function(x, conf.level=.95) {
  sd <- sd(x)
  n <- length(x)
  z <- qnorm(p=(1-conf.level)/2, lower.tail=FALSE)
  lowerBound <- mean(x) - z*sd/sqrt(n)
  upperBound <- mean(x) + z*sd/sqrt(n)

  if (any(is.na(c(lowerBound, upperBound)))) {
    ci <- list('lowerBound'=jsonlite::unbox(NA), 'upperBound'=jsonlite::unbox(NA), 'error'=jsonlite::unbox("Failed to determine confidence interval for mean."))
  } else {
    ci <- list('lowerBound'=jsonlite::unbox(lowerBound), 'upperBound'=jsonlite::unbox(upperBound), 'error'=jsonlite::unbox(""))
  }
  

  return(list(ci))
}

geometricMeanCI <- function(x, conf.level=.95) {
  if (any(x <= 0)) { warning("Zero and negative values will be ignored when finding the confidence interval for the geometric mean.") }

  x <- x[x>0]

  r <- mean(log(x))
  n <- length(x)
  SEr <- sqrt((sum((log(x)-r)^2))/(n-1))/sqrt(n)
  upperBound <- exp(r+2*SEr)
  lowerBound <- exp(r-2*SEr)

  if (any(is.na(c(lowerBound, upperBound)))) {
    ci <- list('lowerBound'=jsonlite::unbox(NA), 'upperBound'=jsonlite::unbox(NA), 'error'=jsonlite::unbox("Failed to determine confidence interval for geometric mean."))
  } else {
    ci <- list('lowerBound'=jsonlite::unbox(lowerBound), 'upperBound'=jsonlite::unbox(upperBound), 'error'=jsonlite::unbox(""))
  }
  

  return(list(ci))
}

proportionCI <- function(numerator, denominator, conf.level=.95) {
  p <- numerator/denominator
  n <- denominator
  z <- qnorm(p=(1-conf.level)/2, lower.tail=FALSE)
  lowerBound <- p - z*sqrt(p*(1-p)/n)
  lowerBound <- ifelse(lowerBound < 0, 0, lowerBound)
  upperBound <- p + z*sqrt(p*(1-p)/n)
  upperBound <- ifelse(upperBound > 1, 1, upperBound)

  if (any(is.na(c(lowerBound, upperBound))) || any(!length(c(lowerBound, upperBound)))) {
    ci <- list('lowerBound'=jsonlite::unbox(NA), 'upperBound'=jsonlite::unbox(NA), 'error'=jsonlite::unbox("Failed to determine confidence interval for proportion."))
  } else {
    ci <- list('lowerBound'=jsonlite::unbox(lowerBound), 'upperBound'=jsonlite::unbox(upperBound), 'error'=jsonlite::unbox(""))
  }

  return(list(ci))
}

simpleSampleSize <- function(x) {
  list(list("N" = jsonlite::unbox(length(x))))
}

formatSimpleSampleSize <- function(x) {
  if (length(x) > 1) stop("`x` must be a single number representing the number of samples.")
  list(list("N" = jsonlite::unbox(x)))
}

proportionSampleSize <- function(numerator, denominator) {
  list(list("numeratorN"=jsonlite::unbox(length(numerator)), 
            "denominatorN"=jsonlite::unbox(length(denominator))))
}

formatProportionSampleSize <- function(numerator, denominator) {
  if (length(numerator) > 1 || length(denominator) > 1) stop("`numerator` and `denominator` must be single numbers representing the number of samples.")
  list(list("numeratorN"=jsonlite::unbox(numerator), 
            "denominatorN"=jsonlite::unbox(denominator)))
}

roundedSD <- function(x, digits = 4, ...) {
  as.list(round(stats::sd(x, ...), digits))
}

roundedMean <- function(x, digits = 4, ...) {
  c(round(mean(x, ...), digits))
} 

roundedGeometricMean <- function(x, digits = 4, ...) {
  if (any(x <= 0)) { warning("Zero and negative values will be ignored when finding geometric mean.") }

  c(round(exp(mean(log(x[x>0]))), digits))
}

roundedMedian <- function(x, digits = 4, ...) {
  c(round(median(x, ...), digits))
}

roundedQuantile <- function(x, digits = 4, ...) {
  as.list(round(stats::quantile(x, ...), digits))
}

roundedRatio <- function(numerator, denominator, digits = 4, NaNValue = NA, ...) {
  ratio <- numerator/denominator
  ratio <- round(ratio, digits)
  ratio[is.nan(ratio)] <- NaNValue
  return(c(ratio))
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
  lowerfence <- min(x[x >= lowerfence])
  upperfence <- summary[4] + (1.5*iqr)
  upperfence <- max(x[x <= upperfence])

  return(list(lowerfence, upperfence))
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
densityCurve <- function(x) {
  if (!length(x) || all(is.na(x))) { return(list('densityX'=numeric(0),
                                                 'densityY'=numeric(0))) }
  curve <- stats::density(x)
  return(list("densityX" = c(curve$x), "densityY" = c(curve$y)))
}

# Prediction data frame
# Get predictions with standard errors into data frame
#
# @keyword internal
# @alias predictdf.gam
# @alias predictdf.loess
#' @importFrom stats qnorm
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

predictdf.default <- function(model, xseq, se = FALSE, level = .95) {
  pred <- stats::predict(model, newdata = veupathUtils::new_data_frame(list(x = xseq)), se.fit = se,
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
  pred <- stats::predict(model, newdata = veupathUtils::data_frame(x = xseq), se = se)

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
  pred <- stats::predict(model, newdata = veupathUtils::data_frame(x = xseq), se.fit = se,
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
  if (sum(complete.cases(dt)) == 0) { return(data.table::data.table('bestFitLineX'=numeric(0),
                                                                   'bestFitLineY'=numeric(0),
                                                                   'r2'=NA))}
  xseq <- sort(unique(dt$x))
  linearModel <- stats::lm(y ~ x, dt)

  bestFitLine <- data.table::as.data.table(predictdf(linearModel, xseq))
  bestFitLine$x <- as.character(bestFitLine$x)
  if (collapse) {
    bestFitLine <- bestFitLine[, lapply(.SD, list)]
  }
  data.table::setnames(bestFitLine, c('bestFitLineX', 'bestFitLineY'))
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
  if (inherits(dt$x, 'Date')) {
    dateMap <- data.table::data.table('date'=dt$x, 'numeric'=as.numeric(dt$x))
    dt$x <- as.numeric(dt$x)
  }
  xseq <- sort(unique(dt$x))

  if (method == 'loess') {
    smoothed <- try(stats::loess(y ~ x, dt), silent = TRUE)
  } else if (method == 'gam') {
    smoothed <- try(mgcv::gam(y ~ s(x, bs = "cs"), data = dt, method = "REML"), silent = TRUE)
  } else {
    stop('Unrecognized smoothing method.')
  }

  if (any(veupathUtils::is.error(smoothed))) {
    dt <- data.table::data.table("smoothedMeanX" = list(numeric()), "smoothedMeanY" = list(numeric()), "smoothedMeanSE" = list(numeric()), "smoothedMeanError" = jsonlite::unbox(as.character(smoothed[1])))
  } else {
    smoothed <- try(data.table::as.data.table(predictdf(smoothed, xseq)))
    if (any(veupathUtils::is.error(smoothed))) {
      dt <- data.table::data.table("smoothedMeanX" = list(numeric()), "smoothedMeanY" = list(numeric()), "smoothedMeanSE" = list(numeric()), "smoothedMeanError" = jsonlite::unbox(as.character(smoothed[1]))) 
    } else {
      if (exists('dateMap')) {
      smoothed$x <- dateMap[match(smoothed$x, dateMap$numeric),]$date
    }

    if (collapse) {
      dt <- data.table::data.table("smoothedMeanX" = list(as.character(smoothed$x)), "smoothedMeanY" = list(smoothed$y), "smoothedMeanSE" = list(smoothed$se), "smoothedMeanError" = jsonlite::unbox(""))
    } else {
      dt <- data.table::data.table("smoothedMeanX" = as.character(smoothed$x), "smoothedMeanY" = smoothed$y, "smoothedMeanSE" = smoothed$se, "smoothedMeanError" = jsonlite::unbox(""))
    }
    }
  }

  return(dt)
}

chiSq <- function(tbl, collapse = TRUE) {
  if (!length(tbl)) {
    return(data.table::data.table('chisq'=jsonlite::unbox(NA), 
                                  'pvalue'=jsonlite::unbox(NA), 
                                  'degreesFreedom'=jsonlite::unbox(NA)))
  }

  chisq <- chisq.test(tbl)
  dt <- data.table::data.table('chisq'=jsonlite::unbox(as.numeric(chisq$statistic)), 
                               'pvalue'=jsonlite::unbox(as.numeric(chisq$p.value)), 
                               'degreesFreedom'=jsonlite::unbox(as.numeric(chisq$parameter)))

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

# Compute appropriate nonparametric test comparing multiple distributions.
#' @importFrom stats kruskal.test
#' @importFrom stats wilcox.test
nonparametricTest <- function(values, groups) {
  
  # values and groups should be vectors of the same length
  if (!identical(length(values), length(groups))) {
    result <- NULL
    return(result)
  }

  # values should be numeric
  if (!is.numeric(values)) {
    stop("values vector should contain numbers.")
  }

  # If there are only 2 groups, use Wilcoxon rank sum. Otherwise use Kruskal–Wallis
  if (uniqueN(groups) == 2) {
    testResult <- try(wilcox.test(values[groups == unique(groups)[1]], values[groups == unique(groups)[2]], conf.level = 0.95, paired=F), silent = TRUE)
  } else {
    testResult <- try(kruskal.test(values, groups), silent = TRUE)
  }
  
  if (veupathUtils::is.error(testResult)){
    testResult <- list("statistic" = numeric(),
      "pvalue" = numeric(),
      "parameter" = numeric(),
      "method" = character(),
      "statsError" = jsonlite::unbox(as.character(testResult[1])))
  } else {
    testResult$parameter <- as.numeric(testResult$parameter)
    testResult$statistic <- as.numeric(testResult$statistic)

    names(testResult)[names(testResult) == 'p.value'] <- 'pvalue'
    testResult <- list("statistic" = jsonlite::unbox(testResult$statistic),
      "pvalue" = jsonlite::unbox(testResult$pvalue),
      "parameter" = testResult$parameter,
      "method" = jsonlite::unbox(testResult$method),
      "statsError" = jsonlite::unbox(""))
  }
  
  return(testResult)
}

# Compute statistics for values in numericCol based on levelsCol, split by byCols
nonparametricByGroup <- function(data, numericCol, levelsCol, byCols = NULL) {
  
  data.table::setDT(data)
  
  if (is.null(byCols)) {
    dt <- data.table::as.data.table(t(nonparametricTest(data[[numericCol]], data[[levelsCol]])))
  } else {
    dt <- data[, {testResult <- nonparametricTest(get(..numericCol), get(..levelsCol));
                    list(statistic=testResult$statistic,
                          pvalue=testResult$pvalue,
                          parameter=testResult$parameter,
                          method=testResult$method,
                          statsError=testResult$statsError)} , by=eval(colnames(data)[colnames(data) %in% byCols])]
  }

  return (dt)
  
}



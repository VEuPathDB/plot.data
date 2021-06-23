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

  if (class(smoothed) == 'try-error') {
    dt <- data.table::data.table("smoothedMeanX" = list(numeric()), "smoothedMeanY" = list(numeric()), "smoothedMeanSE" = list(numeric()), "smoothedMeanError" = jsonlite::unbox(as.character(smoothed[1])))
  } else {
    smoothed <- data.table::as.data.table(predictdf(smoothed, xseq))

    if (exists('dateMap')) {
      smoothed$x <- dateMap[match(smoothed$x, dateMap$numeric),]$date
    }

    if (collapse) {
      dt <- data.table::data.table("smoothedMeanX" = list(as.character(smoothed$x)), "smoothedMeanY" = list(smoothed$y), "smoothedMeanSE" = list(smoothed$se), "smoothedMeanError" = jsonlite::unbox(""))
    } else {
      dt <- data.table::data.table("smoothedMeanX" = as.character(smoothed$x), "smoothedMeanY" = smoothed$y, "smoothedMeanSE" = smoothed$se, "smoothedMeanError" = jsonlite::unbox(""))
    }
  }

  return(dt)
}

#' Odds Ratio
#'
#' This function calculates odds ratio, confidence intervals and p-values for epidemiologic data 
#' @param tbl A frequency table of two binary variables.
#' @return data.table
#' @export
oddsRatio <- function(tbl) {
  a <- tbl[1,1]
  b <- tbl[2,1]
  c <- tbl[1,2]
  d <- tbl[2,2]

  OR <- (a*d)/(b*c)
  OR <- round(OR, digits=4)
  alpha <- 0.05
  siglog <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  logloOR <- logOR - zalph * siglog
  loghiOR <- logOR + zalph * siglog
  ORlo <- round(exp(logloOR), digits=4)
  ORhi <- round(exp(loghiOR), digits=4)

  p <- chisq.test(tbl)
  p <- p$p.value
  p <- round(p, digits=4)
 
  dt <- data.table::data.table('oddsratio'=jsonlite::unbox(OR), 'orInterval'=jsonlite::unbox(paste0(ORlo, "-", ORhi)), 'pvalue'=jsonlite::unbox(p))

  return(dt)
}

#' Relative Risk
#'
#' This function calculates relative risk, confidence intervals and p-values for epidemiologic data 
#' @param tbl A frequency table of two binary variables.
#' @return data.table
#' @export
relativeRisk <- function(tbl) {
  a <- tbl[1,1]
  b <- tbl[2,1]
  c <- tbl[1,2]
  d <- tbl[2,2]

  RR <- (a/(a+b)) / (c/(c+d))
  RR <- round(RR, digits=4)
  alpha <- 0.05
  siglog <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
  zalph <- qnorm(1 - alpha/2)
  logRR <- log(RR)
  logloRR <- logRR - zalph * siglog
  loghiRR <- logRR + zalph * siglog
  RRlo <- round(exp(logloRR), digits=4)
  RRhi <- round(exp(loghiRR), digits=4)

  p <- chisq.test(tbl)
  p <- p$p.value
  p <- round(p, digits=4)

  dt <- data.table::data.table('relativerisk'=jsonlite::unbox(RR), 'rrInterval'=jsonlite::unbox(paste0(RRlo, "-", RRhi)), 'pvalue'=jsonlite::unbox(p))

  return(dt)
}

bothRatios <- function(tbl, collapse = TRUE) {
  a <- tbl[1,1]
  b <- tbl[2,1]
  c <- tbl[1,2]
  d <- tbl[2,2]

  OR <- (a*d)/(b*c)
  RR <- (a/(a+b)) / (c/(c+d))
  OR <- round(OR, digits=4)
  RR <- round(RR, digits=4)

  alpha <- 0.05
  siglog <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  logRR <- log(RR)
  logloOR <- logOR - zalph * siglog
  logloRR <- logRR - zalph * siglog
  loghiOR <- logOR + zalph * siglog
  loghiRR <- logRR + zalph * siglog

  ORlo <- round(exp(logloOR), digits=4)
  RRlo <- round(exp(logloRR), digits=4)
  ORhi <- round(exp(loghiOR), digits=4)
  RRhi <- round(exp(loghiRR), digits=4)

  p <- chisq.test(tbl)
  p <- p$p.value
  p <- round(p, digits=4)

  dt <- data.table::data.table('oddsratio'=jsonlite::unbox(OR), 'relativerisk'=jsonlite::unbox(RR), 'orInterval'=jsonlite::unbox(paste0(ORlo, '-', ORhi)), 'rrInterval'=jsonlite::unbox(paste0(RRlo, '-', RRhi)), 'pvalue'=jsonlite::unbox(p))

  return(dt)
}

chiSq <- function(tbl, collapse = TRUE) {
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
  dt$chisq <- jsonlite::unbox(chisq$statistic)
  dt$pvalue <- jsonlite::unbox(chisq$p.value)
  dt$degreesFreedom <- jsonlite::unbox(chisq$parameter)

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
  
  if (class(testResult) == 'try-error'){
    testResult <- list("statistic" = numeric(),
      "pvalue" = numeric(),
      "parameter" = numeric(),
      "method" = character(),
      "statsError" = jsonlite::unbox(as.character(testResult[1])))
    testResult <- list(testResult)
  } else {
    testResult$parameter <- as.numeric(testResult$parameter)
    names(testResult)[names(testResult) == 'p.value'] <- 'pvalue'
    testResult <- list(c(testResult[c('statistic', 'pvalue', 'parameter', 'method')], "statsError" = jsonlite::unbox("")))
  }
  
  return(testResult)
}

# Compute statistics for values in numericCol based on levelsCol, split by byCols
nonparametricByGroup <- function(data, numericCol, levelsCol, byCols = NULL) {
  
  setDT(data)
  
  if (is.null(byCols)) {
    statsResults <- data.table::as.data.table(t(nonparametricTest(data[[numericCol]], data[[levelsCol]])))
  } else {
    statsResults <- data[, .(nonparametricTest(get(..numericCol), get(..levelsCol))) , by=eval(colnames(data)[colnames(data) %in% byCols])]
  }
  
  data.table::setnames(statsResults, 'V1', 'statistics')

  return (statsResults)
  
}



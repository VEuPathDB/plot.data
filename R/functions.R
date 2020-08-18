## TODO reorganize these, maybe helpers into a separate file ??

getAggStr <- function(col, group, panel) {
  aggStr <- paste(c(col, paste(c(group,panel), collapse=" + ")), collapse=" ~ ")

  return(aggStr)
}


## TODO optimize aggregate calls, probably with data.table syntax
# data is a data.table
# col is a column name we want a summary for
# group is a column name for the aggregation key within a panel
# panel is a column name for the strata/ facet
groupSummary <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(quantile(data[[col]]),4)))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  names(dt) <- c(group, panel, 'min', 'q1', 'median', 'q3', 'max')

  return(dt)
}


fences <- function(x) {
  summary <- quantile(x)
  iqr <- summary[4] - summary[2]
  lowerfence <- summary[2] - (1.5*iqr)
  upperfence <- summary[4] + (1.5*iqr)

  return(c(lowerfence, upperfence))
}

## TODO groupFences to return lowerfence and upperfence for boxplots
groupFences <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(fences(data[[col]])))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  names(dt) <- c(group, panel, 'lowerfence', 'upperfence')

  return(dt)
}


groupMean <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(mean(data[[col]],4))))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  names(dt) <- c(group, panel, 'mean')

  return(dt)
}


groupSD <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(sd(data[[col]],4))))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(sd(x),4)}))
  }

  names(dt) <- c(group, panel, 'sd')

  return(dt)
}


groupSize <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(length(data[[col]])))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, length))
  }

  names(dt) <- c(group, panel, 'size')

  return(dt)
}


outliers <- function(x) {
  fences <- fences(x)

  return(x[x < fences[1] | x > fences[2]])
}

groupOutliers <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  names(dt) <- c(group, panel, 'outliers')

  return(dt)
}


## TODO consider renaming x and y to independent and dependent
densityCurve <- function(x) {
  curve <- density(x)
  
  return(data.table("x" = list(curve$x), "y" = list(curve$y)))
}

groupDensity <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
  }

  names(dt) <- c(group, panel, 'x', 'y')

  return(dt)
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

# assumes that dt has an x and y column
smoothedMean <- function(dt, method) {
  xseq <- sort(unique(dt$x))

  if (method == 'loess') {
    smoothed <- loess(y ~ x, dt)
    smoothed <- as.data.table(predictdf.loess(smoothed, xseq))
  } else if (method == 'gam') {
    smoothed <- mgcv::gam(y ~ s(x, bs = "cs"), data = dt, method = "REML")
    smoothed <- as.data.table(predictdf.gam(smoothed, xseq))
  } else {
    stop('Unrecognized smoothing method.')
  }

  return(data.table("x" = list(smoothed$x), "y" = list(smoothed$y), "ymin" = list(smoothed$ymin), "ymax" = list(smoothed$ymax), "se" = list(smoothed$se)))
}

## TODO from here down optimize !!
groupSmoothedMean <- function(data, x, y, group = NULL, panel = NULL) {
  names(data)[names(data) == y] <- 'y'
  names(data)[names(data) == x] <- 'x'
  y <- 'y'
  x <- 'x'
  aggStr <- getAggStr(y, group, panel)

  maxGroupSize <- max(groupSize(data, y, group, panel)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }


  if (aggStr == col) {
    dt <- smoothedMean(data, method)
  } else {
    dt.list <- split(data, list(data[[group]], data[[panel]]))
    dt.list <- lapply(dt.list, smoothedMean, method)
    dt <- reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    dt$group <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
    dt$panel <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
    dt$name <- NULL
  }

  return(dt)
} 

# bin based on whole dataset so they are consistent across groups and panels
bin <- function(x, binWidth) {
  summary <- quantile(x)
  bounds <- c(summary[1], summary[5])
  bounds <- sign(bounds) * ceiling(abs(bounds) * 100) / 100
  breaks <- seq(bounds[1], bounds[2], binWidth)
  breaks <- c(breaks, (breaks[length(breaks)] + binWidth))

  return(as.character(cut(x, breaks=breaks)))
}

#get size/ proportion per group per panel
## TODO consider using noStatsFacet to generate the lists
binSize <- function(data, col, group = NULL, panel = NULL, binWidth) {
  aggStr <- getAggStr(col, group, panel)
  aggStr2 <- paste(c(aggStr, 'x'), collapse = " + ")
  aggStr3 <- getAggStr('x', group, panel)

  data$x <- bin(data[[col]], binWidth)

  if (aggStr == col) {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt <- data.table('x' = list(dt$x), 'y' = list(dt[[col]]))
  } else {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt2 <- aggregate(as.formula(aggStr), dt, list)
    dt3 <- aggregate(as.formula(aggStr3), dt, list)
    mergeByCols <- c(group, panel)
    dt <- merge(dt2, dt3, by = mergeByCols)
    names(dt) <- c(group, panel, 'y', 'x')
  }

  return(dt)
}

binProportion <- function(data, col, group = NULL, panel = NULL, binWidth) {
  aggStr <- getAggStr(col, group, panel)
  aggStr2 <- paste(c(aggStr, 'x'), collapse=" + ")
  aggStr3 <- getAggStr('x', group, panel)
  
  data$x <- bin(data[[col]], binWidth)

  if (aggStr == col) {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt$denom <- length(data[[col]])
    dt <- data.table('x' = list(dt$x), 'y' = list(dt[[col]]/dt$denom))
  } else {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt2 <- aggregate(as.formula(aggStr), data, length)
    names(dt2) <- c(group, panel, 'denom')
    mergeByCols <- c(group, panel)
    dt2 <- merge(dt, dt2, by = mergeByCols)
    dt2[[col]] <- dt2[[col]]/dt2$denom
    dt2 <- aggregate(as.formula(aggStr), dt2, list)
    dt3 <- aggregate(as.formula(aggStr3), dt, list)
    dt <- merge(dt2, dt3, by = mergeByCols)
    names(dt) <- c(group, panel, 'y', 'x')
  }

  return(dt)
}


noStatsFacet <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table(col = list(data[[col]]))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, list))
  }

  return(dt)
}


epitabToDT <- function(m, method) {
  dt <- as.data.table(m)
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

oddsRatio <- function(group, col) {
  m <- epitab(group, col, method = "oddsratio")$tab
  dt <- epitabToDT(m, 'oddsratio')
 
  return(dt)
}

relativeRisk <- function(group, col) {
  m <- epitab(group, col, method = "riskratio")$tab
  dt <- epitabToDT(m, 'relativerisk')

  return(dt)
}

panelOddsRatio <- function(data, col, group, panel = NULL) {

  if (is.null(panel)) {
    dt <- oddsRatio(data[[group]], data[[col]])
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, oddsRatio)
    dt <- reduce(dt.list, rbind)
    dt$panel <- names(dt.list)    
  }

  return(dt)
}

panelRelativeRisk <- function(data, col, group, panel = NULL) {

  if (is.null(panel)) {
    dt <- relativeRisk(data[[group]], data[[col]])
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, relativeRisk)
    dt <- reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

groupSummary <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(round(quantile(data[[col]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  names(dt) <- c(group, panel, 'min', 'q1', 'median', 'q3', 'max')

  return(dt)
}

groupFences <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(fences(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  names(dt) <- c(group, panel, 'lowerfence', 'upperfence')

  return(dt)
}

groupMean <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(round(mean(data[[col]],4))))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  names(dt) <- c(group, panel, 'mean')

  return(dt)
}

groupSD <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(round(sd(data[[col]],4))))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(stats::sd(x),4)}))
  }

  names(dt) <- c(group, panel, 'sd')

  return(dt)
}

groupSize <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(length(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
  }

  names(dt) <- c(group, panel, 'size')

  return(dt)
}

groupOutliers <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  names(dt) <- c(group, panel, 'outliers')

  return(dt)
}

groupDensity <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
  }

  names(dt) <- c(group, panel, 'x', 'y')

  return(dt)
}

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
    dt <- purrr::reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    dt$group <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
    dt$panel <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
    dt$name <- NULL
  }

  return(dt)
}

noStatsFacet <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- data.table::data.table(col = list(data[[col]]))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, list))
  }

  return(dt)
}



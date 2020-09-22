# TODO profile and optimize
# consider changing x and y to independent and dependent

groupSummary <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(quantile(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  names(dt) <- c(x, group, panel, 'min', 'q1', 'median', 'q3', 'max')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupFences <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(fences(data[[y]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  names(dt) <- c(x, group, panel, 'lowerfence', 'upperfence')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupMean <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(mean(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  names(dt) <- c(x, group, panel, 'mean')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupSD <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(sd(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(stats::sd(x),4)}))
  }

  names(dt) <- c(x, group, panel, 'sd')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupSize <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(length(data[[y]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
  }

  names(dt) <- c(x, group, panel, 'size')

  return(dt)
}

groupOutliers <- function(data, x = NULL, y, group = NULL, panel = NULL) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  names(dt) <- c(x, group, panel, 'outliers')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupDensity <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, c(group, panel))

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
    dt <- noStatsFacet(dt)
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
  }

  names(dt) <- c(group, panel, 'x', 'y')

  return(dt)
}

#' @importFrom purrr reduce
groupSmoothedMean <- function(data, x, y, group = NULL, panel = NULL) {
  names(data)[names(data) == y] <- 'y'
  names(data)[names(data) == x] <- 'x'
  y <- 'y'
  x <- 'x'
  aggStr <- getAggStr(y, c(group, panel))

  maxGroupSize <- max(groupSize(data, x = NULL, y, group, panel)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }

  if (aggStr == y) {
    dt <- smoothedMean(data, method)
  } else {
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    dt.list <- lapply(dt.list, smoothedMean, method)
    dt <- purrr::reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    if (is.null(group)) {
      dt$panel <- dt$name
    } else {
      dt$group <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      dt$panel <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (all(is.na(dt$panel))) { dt$panel <- NULL }
    }
    dt$name <- NULL
  }

  return(dt)
}

noStatsFacet <- function(data, group = NULL, panel = NULL) {
  if (class(data)[1] != "data.table") {
    data <- as.data.table(data)
  }

  if (is.null(group) && is.null(panel)) {
    dt <- data[, lapply(.SD, list)]
  } else {   
    dt <- data[, lapply(.SD, list), by=eval(colnames(data)[colnames(data) %in% c(group, panel)])]
  }

  return(dt)
}

groupSplit <- function(data, x, y, z, group, panel, longToWide = FALSE) {
  aggStr <- getAggStr(c(group, panel, y), x)

  if (!is.null(group) & !is.null(panel)) {
    if (longToWide) {
      dt <- data.table::dcast(data, as.formula(aggStr), value.var="z")
    } else {
      dt <- data.table::data.table('table' = list(data), 'name' = 'DUMMY')
    }
  } else {
    if (longToWide) {
      data <- data.table::dcast(data, as.formula(aggStr), value.var="z")
    }
    # TODO consider if we can make a helper for dt.list
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    names <- names(dt.list)
    dt.list <- lapply(dt.list, removeGroupPanel, group, panel)
    dt <- data.table::data.table('name' = names(dt.list), 'table' = dt.list)
    if (is.null(group)) {
      dt$panel <- dt$name
    } else {
      dt$group <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      dt$panel <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (all(is.na(dt$panel))) { dt$panel <- NULL }
    }
    dt$name <- NULL
  }

  return(dt)
}

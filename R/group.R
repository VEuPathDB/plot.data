groupSummary <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(quantile(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  names(dt) <- c(independent, group, panel, 'min', 'q1', 'median', 'q3', 'max')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupFences <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(fences(data[[dependent]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  names(dt) <- c(independent, group, panel, 'lowerfence', 'upperfence')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupMean <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(mean(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  names(dt) <- c(independent, group, panel, 'mean')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupSD <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(sd(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(stats::sd(x),4)}))
  }

  names(dt) <- c(independent, group, panel, 'sd')
  dt <- noStatsFacet(dt, group, panel)

  return(dt)
}

groupSize <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(length(data[[dependent]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
  }
  names(dt) <- c(independent, group, panel, 'size')
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

groupOutliers <- function(data, independent = NULL, dependent, group = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, group, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  names(dt) <- c(independent, group, panel, 'outliers')
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
  names(dt) <- c(group, panel, 'independent', 'dependent')
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

#' @importFrom purrr reduce
groupSmoothedMean <- function(data, independent, dependent, group = NULL, panel = NULL) {
  names(data)[names(data) == dependent] <- 'dependent'
  names(data)[names(data) == independent] <- 'independent'
  dependent <- 'dependent'
  independent <- 'independent'
  aggStr <- getAggStr(dependent, c(group, panel))

  maxGroupSize <- max(groupSize(data, NULL, dependent, group, panel)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }

  if (aggStr == dependent) {
    dt <- smoothedMean(data, method)
  } else {
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    dt.list <- lapply(dt.list, smoothedMean, method)
    dt <- purrr::reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    if (is.null(group)) {
      dt[[panel]] <- dt$name
    } else {
      dt[[group]] <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      panelNames <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (!all(is.na(panel))) { dt[[panel]] <- panelNames }
    }
    dt$name <- NULL
  }
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

noStatsFacet <- function(data, group = NULL, panel = NULL) {
  if (class(data)[1] != "data.table") {
    data <- data.table::setDT(data)
  }

  if (is.null(group) && is.null(panel)) {
    dt <- data[, lapply(.SD, list)]
  } else {   
    dt <- data[, lapply(.SD, list), by=eval(colnames(data)[colnames(data) %in% c(group, panel)])]
  }
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

# consider removing group here, if this is only for heatmap
# think we want table col reformatted to be two cols, 'label' and 'value'. the second will be a list.
# can we use noStatsFacet for the second task ??

groupSplit <- function(data, independent, dependent, z, group, panel, longToWide = FALSE) {
  aggStr <- getAggStr(c(group, panel, dependent), independent)

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
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    names <- names(dt.list)
    dt.list <- lapply(dt.list, removeGroupPanel, group, panel)
    dt <- data.table::data.table('name' = names(dt.list), 'table' = dt.list)
    if (is.null(group)) {
      dt[[panel]] <- dt$name
    } else {
      dt[[group]] <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      panelNames <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (!all(is.na(panel))) { dt[[panel]] <- panelNames }
    }
    dt$name <- NULL
  }
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

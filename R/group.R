groupSummary <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(quantile(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  names(dt) <- c(independent, overlay, panel, 'min', 'q1', 'median', 'q3', 'max')
  dt <- noStatsFacet(dt, overlay, panel)

  return(dt)
}

groupFences <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(fences(data[[dependent]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  names(dt) <- c(independent, overlay, panel, 'lowerfence', 'upperfence')
  dt <- noStatsFacet(dt, overlay, panel)

  return(dt)
}

groupMean <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(mean(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  names(dt) <- c(independent, overlay, panel, 'mean')
  dt <- noStatsFacet(dt, overlay, panel)

  return(dt)
}

groupSD <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(round(sd(data[[dependent]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(stats::sd(x),4)}))
  }

  names(dt) <- c(independent, overlay, panel, 'sd')
  dt <- noStatsFacet(dt, overlay, panel)

  return(dt)
}

groupSize <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(length(data[[dependent]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
  }
  names(dt) <- c(independent, overlay, panel, 'size')
  indexCols <- c(panel, overlay)
  setkeyv(dt, indexCols)
  
  return(dt)
}

groupOutliers <- function(data, independent = NULL, dependent, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(dependent, c(independent, overlay, panel))

  if (aggStr == dependent) {
    dt <- data.table::as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  names(dt) <- c(independent, overlay, panel, 'outliers')
  dt <- noStatsFacet(dt, overlay, panel)

  return(dt)
}

groupDensity <- function(data, col, overlay = NULL, panel = NULL) {
  aggStr <- getAggStr(col, c(overlay, panel))

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
    dt <- noStatsFacet(dt)
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
  }
  names(dt) <- c(overlay, panel, 'independent', 'dependent')
  indexCols <- c(panel, overlay)
  setkeyv(dt, indexCols)
  
  return(dt)
}

#' @importFrom purrr reduce
groupSmoothedMean <- function(data, independent, dependent, overlay = NULL, panel = NULL) {
  names(data)[names(data) == dependent] <- 'dependent'
  names(data)[names(data) == independent] <- 'independent'
  dependent <- 'dependent'
  independent <- 'independent'
  aggStr <- getAggStr(dependent, c(overlay, panel))

  maxGroupSize <- max(groupSize(data, NULL, dependent, overlay, panel)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }

  if (aggStr == dependent) {
    dt <- smoothedMean(data, method)
  } else {
    colsList <- getInteractionColsList(data, overlay, panel)
    dt.list <- split(data, colsList)
    dt.list <- lapply(dt.list, smoothedMean, method)
    dt <- purrr::reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    if (is.null(overlay)) {
      dt[[panel]] <- dt$name
    } else {
      dt[[overlay]] <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      panelNames <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (!all(is.na(panel))) { dt[[panel]] <- panelNames }
    }
    dt$name <- NULL
  }
  indexCols <- c(panel, overlay)
  setkeyv(dt, indexCols)
  
  return(dt)
}

noStatsFacet <- function(data, overlay = NULL, panel = NULL) {
  if (class(data)[1] != "data.table") {
    data <- data.table::setDT(data)
  }

  if (is.null(overlay) && is.null(panel)) {
    dt <- data[, lapply(.SD, list)]
  } else {   
    dt <- data[, lapply(.SD, list), by=eval(colnames(data)[colnames(data) %in% c(overlay, panel)])]
  }
  indexCols <- c(panel, overlay)
  setkeyv(dt, indexCols)
  
  return(dt)
}

# consider removing group here, if this is only for heatmap
# think we want table col reformatted to be two cols, 'label' and 'value'. the second will be a list.
# can we use noStatsFacet for the second task ??

groupSplit <- function(data, independent, dependent, z, overlay, panel, longToWide = FALSE) {
  aggStr <- getAggStr(c(overlay, panel, dependent), independent)

  if (!is.null(overlay) & !is.null(panel)) {
    if (longToWide) {
      dt <- data.table::dcast(data, as.formula(aggStr), value.var="z")
    } else {
      dt <- data.table::data.table('table' = list(data), 'name' = 'DUMMY')
    }
  } else {
    if (longToWide) {
      data <- data.table::dcast(data, as.formula(aggStr), value.var="z")
    }
    colsList <- getInteractionColsList(data, overlay, panel)
    dt.list <- split(data, colsList)
    names <- names(dt.list)
    dt.list <- lapply(dt.list, removeGroupPanel, overlay, panel)
    dt <- data.table::data.table('name' = names(dt.list), 'table' = dt.list)
    if (is.null(overlay)) {
      dt[[panel]] <- dt$name
    } else {
      dt[[overlay]] <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
      panelNames <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
      if (!all(is.na(panel))) { dt[[panel]] <- panelNames }
    }
    dt$name <- NULL
  }
  indexCols <- c(panel, overlay)
  setkeyv(dt, indexCols)
  
  return(dt)
}

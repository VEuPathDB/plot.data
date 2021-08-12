groupSummary <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, {quantile <- roundedQuantile(get(..y));
                list(min = quantile[[1]],
                     q1 = quantile[[2]],
                     median = quantile[[3]],
                     q3 = quantile[[4]],
                     max = quantile[[5]])},
               keyby = eval(byCols)]

  if(collapse){
    dt <- collapseByGroup(dt, group, panel)
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

groupFences <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, {fences <- fences(get(..y));
                list(lowerfence = fences[[1]],
                     upperfence = fences[[2]])},
               keyby = eval(byCols)] 
 
  if(collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

groupMean <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, list(mean=roundedMean(get(..y))), keyby=eval(byCols)]

  if(collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

groupSD <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, list(sd=roundedSD(get(..y))), keyby=eval(byCols)]

  if(collapse){
    dt <- collapseByGroup(dt, group, panel)
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

groupSize <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, list(size=length(get(..y))), keyby=eval(byCols)]

  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
  
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}


groupProportion <- function(data, x = NULL, y, group = NULL, panel = NULL, barmode = 'group', collapse=T) {

  numCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  # byCols determine the denominator of the proportion calculation
  if (barmode == 'group') {
    denomCols <- colnames(data)[colnames(data) %in% c(group, panel)]
  } else if (barmode == 'stack') {
    denomCols <- colnames(data)[colnames(data) %in% c(x, panel)]
  } else {
    stop('Options for barmode are "stack" or "group".')
  }
 
  denom <- data[, list(sum=sum(get(..y))), by=eval(denomCols)]
  if (!length(denomCols)) {
    data$sum <- denom$sum
  } else {
    data <- merge(denom, data, by=eval(denomCols))
  }
  dt <- data[, list(proportion=sum(get(..y))/sum), keyby=eval(numCols)]
  dt <- unique(dt)

  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
  
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}


groupOutliers <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, lapply(.SD, outliers), keyby=eval(byCols)][, list(outliers=lapply(.SD, as.vector)), keyby=byCols]

  if (length(byCols)) {
    byColValues <- unique(data[, byCols, with=FALSE])
    dt <- merge(dt, byColValues, by=byCols, all=TRUE)
  }

  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
  
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  return(dt)
}

groupDensity <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse = TRUE) {
  byCols <- colnames(data)[colnames(data) %in% c(x, group, panel)]
  dt <- data[, {density <- densityCurve(get(..y));
                list(densityX = density[[1]], 
                     densityY = density[[2]])}, 
               keyby = eval(byCols)]

  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
 
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

#' @importFrom purrr reduce
groupSmoothedMean <- function(data, x, y, group = NULL, panel = NULL, collapse = TRUE) {
  
  data.table::setnames(data, x, 'x')
  data.table::setnames(data, y, 'y')
  y <- 'y'
  x <- 'x'

  maxGroupSize <- max(groupSize(data, NULL, y, group, panel, collapse=F)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) method <- 'gam'

  byCols <- colnames(data)[colnames(data) %in% c(group, panel)]
  if (all(is.null(c(group,panel)))) {
    dt <- data[, {smoothed <- smoothedMean(.SD, method, collapse);
                  list(smoothedMeanX = smoothed[[1]],
                       smoothedMeanY = smoothed[[2]],
                       smoothedMeanSE = smoothed[[3]],
                       smoothedMeanError = smoothed[[4]])}]
  } else {
    dt <- data[, {smoothed <- smoothedMean(.SD, method, collapse);
                  list(smoothedMeanX = smoothed[[1]],
                       smoothedMeanY = smoothed[[2]],
                       smoothedMeanSE = smoothed[[3]],
                       smoothedMeanError = smoothed[[4]])},
                  keyby=eval(byCols)]
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

groupBestFitLine <- function(data, x, y, group = NULL, panel = NULL, collapse = TRUE) {
  
  data.table::setnames(data, x, 'x')
  data.table::setnames(data, y, 'y')
  y <- 'y'
  x <- 'x'
  
  byCols <- colnames(data)[colnames(data) %in% c(group, panel)]
  if (all(is.null(c(group,panel)))) {
    dt <- data[, {bestFitLine <- bestFitLine(.SD, collapse);
                  list(bestFitLineX = bestFitLine[[1]],
                       bestFitLineY = bestFitLine[[2]],
                       r2 = bestFitLine[[3]])}]
  } else {
    dt <- data[, {bestFitLine <- bestFitLine(.SD, collapse);
                  list(bestFitLineX = bestFitLine[[1]],
                       bestFitLineY = bestFitLine[[2]],
                       r2 = bestFitLine[[3]])},
                  keyby=eval(byCols)]
  }

  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  return(dt)
}

# consider removing group here, if this is only for heatmap
# think we want table col reformatted to be two cols, 'label' and 'value'. the second will be a list.
# can we use collapseByGroup for the second task ??

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

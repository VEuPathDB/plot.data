groupSummary <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(quantile(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  data.table::setnames(dt, c(x, group, panel, 'min', 'q1', 'median', 'q3', 'max'))
  if(collapse){
    dt <- collapseByGroup(dt, group, panel)
  }

  return(dt)
}

groupStatistics <- function(data, x, y, group=NULL, panel=NULL, collapse=F) {
  # aggStr <- getAggStr(c(y, group), c(x, panel))
  
  # Statistics will work differently based on if an overlay var is chosen.
  if (is.null(group)) {
    # Then we run stats across x values.
    if (is.null(panel)) {
      # Only one panel to worry about
      statsResults <- data.table::as.data.table(t((runKruskal(.pd[[y]], .pd[[x]]))))
      data.table::setnames(statsResults, c('chisqrd','pvalue'))
    } else {
      # NOT RUN statsResults <- sapply(split(.pd, .pd[[panel]]), function(z) runKruskal(z[[y]], z[[x]])
    }
  } else {
    # Then run stats across overlay values per x per panel
    splitCols <- c(x, panel)
    setDT(data)
    statsResults <- data.table::as.data.table(t(sapply(
      split(data, data[, ..splitCols]), 
      function(v) runKruskal(v[[y]], v[[group]]))), keep.rownames=x)
    # The above is not awesome because we don't really want those ugly 
    # concatenated names
    testCols <- c(y, group)
    dt1 <- collapseByGroup(data, x, panel)
    # dt2 <- lapply(dt1[, ..testCols], FUN=function(v) {runKruskal(v[[y]], v[[group]])})
    dt2 <- dt1[, .(runKruskal(y, group)[1], runKruskal(y, group)[2])]
    dt3 <- data[, sapply(.SD, function(v) {runKruskal(v[[y]], v[[group]])}), by=eval(colnames(data)[colnames(data) %in% c(group, panel)])]
    a <- apply(cbind(dt1[[group]], dt1[[y]]), 1,
               function(v) {runKruskal(v, y, group)})
  }
    #????
    a <- data[, .(runKruskal(.SD, y, group)[1], runKruskal(y, group)[2]), by=panel]
  
}

runKruskal <- function(df, y, g) {
  result <- kruskal.test(df[[y]],df[[g]])
  result <- c(result$statistic, result$p.value)
  return(result)
}

groupFences <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(fences(data[[y]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, fences))
  }

  data.table::setnames(dt, c(x, group, panel, 'lowerfence', 'upperfence'))
  if(collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }

  return(dt)
}

groupMean <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(mean(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  data.table::setnames(dt, c(x, group, panel, 'mean'))
  if(collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }


  return(dt)
}

groupSD <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(round(sd(data[[y]]),4)))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(stats::sd(x),4)}))
  }
  data.table::setnames(dt, c(x, group, panel, 'sd'))
  
  if(collapse){
    dt <- collapseByGroup(dt, group, panel)
  }

  return(dt)
}

groupSize <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(length(data[[y]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
  }
  
  data.table::setnames(dt, c(x, group, panel, 'size'))
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)

  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
  
  return(dt)
}


groupProportion <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))
  
  if (aggStr == y) {
    dt <- data.table::as.data.table(t(1)) # Without any grouping, proportion should always = 1
  } else {

    # Aggregate to get counts of value per group
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, length))
    strataCols <- c(group, panel)
    
    # If there are no strata vars, then we don't need the by term
    if (is.null(strataCols)) {
      dt[, sum := sum(get(y))][, proportion := get(y)/sum]
    } else {
      dt[, sum := sum(get(y)), by=strataCols][, proportion := get(y)/sum]
    }
    
    # Remove unnecessary columns
    dt[, sum := NULL]
    dt[, (y) := NULL]
    
  }
  
  data.table::setnames(dt, c(x, group, panel, 'proportion'))
  indexCols <- c(panel, group)
  setkeyv(dt, indexCols)
  
  if (collapse) {
    dt <- collapseByGroup(dt, group, panel)
  }
  
  return(dt)
}


groupOutliers <- function(data, x = NULL, y, group = NULL, panel = NULL, collapse=T) {
  aggStr <- getAggStr(y, c(x, group, panel))

  if (aggStr == y) {
    dt <- data.table::as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }


  data.table::setnames(dt, c(x, group, panel, 'outliers'))

  if(collapse){
    dt <- collapseByGroup(dt, group, panel)
  }

  return(dt)
}

groupDensity <- function(data, col, group = NULL, panel = NULL, collapse = TRUE) {
  aggStr <- getAggStr(col, c(group, panel))

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
    if (collapse) {
      dt <- collapseByGroup(dt, group, panel)
    }
  } else {
    if (collapse) {
      dt <- data.table::as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
      data.table::setnames(dt, c(group, panel, 'densityX', 'densityY'))
    } else {
      colsList <- getInteractionColsList(data, group, panel)
      dt.list <- split(data, colsList)
      dt.list <- lapply(dt.list, '[[', col)
      dt.list <- lapply(dt.list, densityCurve)
      dt.nrow.list <- lapply(dt.list, nrow)
      dt <- purrr::reduce(dt.list, rbind)
      dt$name <- unlist(lapply(names(dt.list), rep, dt.nrow.list[[1]]))
      if (is.null(group)) {
        dt[[panel]] <- dt$name
      } else {
        dt[[group]] <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
        panelNames <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
        if (!all(is.na(panel))) { dt[[panel]] <- panelNames }
      }
      dt$name <- NULL
    }
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
  aggStr <- getAggStr(y, c(group, panel))

  maxGroupSize <- max(groupSize(data, NULL, y, group, panel, collapse=F)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }

  if (aggStr == y) {
    dt <- smoothedMean(data, method, collapse)
  } else {
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    dt.list <- lapply(dt.list, smoothedMean, method, collapse)
    if (collapse) {
      dt <- purrr::reduce(dt.list, rbind)
      dt$name <- names(dt.list)
    } else {
      dt.nrow.list <- lapply(dt.list, nrow)
      dt <- purrr::reduce(dt.list, rbind)
      dt$name <- unlist(lapply(names(dt.list), rep, dt.nrow.list[[1]]))
    }
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

groupBestFitLine <- function(data, x, y, group = NULL, panel = NULL, collapse = TRUE) {
  
  data.table::setnames(data, x, 'x')
  data.table::setnames(data, y, 'y')
  y <- 'y'
  x <- 'x'
  aggStr <- getAggStr(y, c(group, panel))

  if (aggStr == y) {
    dt <- bestFitLine(data, collapse)
  } else {
    colsList <- getInteractionColsList(data, group, panel)
    dt.list <- split(data, colsList)
    dt.list <- lapply(dt.list, bestFitLine, collapse)
    if (collapse) {
      dt <- purrr::reduce(dt.list, rbind)
      dt$name <- names(dt.list)
    } else {
      dt.nrow.list <- lapply(dt.list, nrow)
      dt <- purrr::reduce(dt.list, rbind)
      dt$name <- unlist(lapply(names(dt.list), rep, dt.nrow.list[[1]]))
    }
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

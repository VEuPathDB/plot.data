binSize <- function(data, col, group = NULL, panel = NULL, binWidth) {
  aggStr <- getAggStr(col, 'x', group, panel)

  data$x <- bin(data[[col]], binWidth)

  if (is.null(group) && is.null(panel)) {
    dt <- aggregate(as.formula(aggStr), data, length)
    dt <- data.table::data.table('x' = list(dt$x), 'y' = list(dt[[col]]))
  } else {
    dt <- aggregate(as.formula(aggStr), data, length)
    dt <- noStatsFacet(dt, col, 'x', group = group, panel = panel)
  }

  return(dt)
}

binProportion <- function(data, col, group = NULL, panel = NULL, binWidth) {
  aggStr <- getAggStr(col, 'x', group, panel)
  aggStr2 <- getAggStr(col, group, panel)

  data$x <- bin(data[[col]], binWidth)

  if (is.null(group) && is.null(panel)) {
    dt <- aggregate(as.formula(aggStr), data, length)
    dt$denom <- length(data[[col]])
    dt <- data.table::data.table('x' = list(dt$x), 'y' = list(dt[[col]]/dt$denom))
  } else {
    dt <- aggregate(as.formula(aggStr), data, length)
    dt2 <- aggregate(as.formula(aggStr2), data, length)
    names(dt2) <- c(group, panel, 'denom')
    mergeByCols <- c(group, panel)
    dt <- merge(dt, dt2, by = mergeByCols)
    dt[[col]] <- dt[[col]]/dt$denom
    dt <- noStatsFacet(dt, 'x', col, group = group, panel = panel)
  }

  return(dt)
}

# TODO consider using noStatsFacet to generate the lists per group

binSize <- function(data, col, group = NULL, panel = NULL, binWidth) {
  aggStr <- getAggStr(col, group, panel)
  if (aggStr == col) {
    aggStr2 <- paste(c(aggStr, 'x'), collapse = " ~ ")
  } else {
    aggStr2 <- paste(c(aggStr, 'x'), collapse = " + ")
  }
  aggStr3 <- getAggStr('x', group, panel)

  data$x <- bin(data[[col]], binWidth)

  if (aggStr == col) {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt <- data.table::data.table('x' = list(dt$x), 'y' = list(dt[[col]]))
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
  if (aggStr == col) {
    aggStr2 <- paste(c(aggStr, 'x'), collapse = " ~ ")
  } else {
    aggStr2 <- paste(c(aggStr, 'x'), collapse = " + ")
  }
  aggStr3 <- getAggStr('x', group, panel)

  data$x <- bin(data[[col]], binWidth)

  if (aggStr == col) {
    dt <- aggregate(as.formula(aggStr2), data, length)
    dt$denom <- length(data[[col]])
    dt <- data.table::data.table('x' = list(dt$x), 'y' = list(dt[[col]]/dt$denom))
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

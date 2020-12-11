binSize <- function(data, col, group = NULL, panel = NULL, binWidth = NULL) {
  aggStr <- getAggStr(col, c('label', 'x', group, panel))

  data$label <- bin(data[[col]], binWidth)
  data$x <- findBinStart(data$label)

  dt <- aggregate(as.formula(aggStr), data, length)
  dt <- noStatsFacet(dt, group, panel)
  names(dt) <- c(group, panel, 'label', 'x', 'y')

  return(dt)
}

binProportion <- function(data, col, group = NULL, panel = NULL, binWidth = NULL) {
  aggStr <- getAggStr(col, c('label', 'x', group, panel))
  aggStr2 <- getAggStr(col, c(group, panel))

  data$label <- bin(data[[col]], binWidth)
  data$x <- findBinStart(data$label)

  dt <- aggregate(as.formula(aggStr), data, length)
  if (is.null(group) && is.null(panel)) {
    dt$denom <- length(data[[col]])
    dt <- data.table::data.table('label' = list(dt$label), 'x' = list(dt$x), 'y' = list(dt[col]/dt$denom))
  } else {
    dt2 <- aggregate(as.formula(aggStr2), data, length)
    names(dt2) <- c(group, panel, 'denom')
    mergeByCols <- c(group, panel)
    dt <- merge(dt, dt2, by = mergeByCols)
    dt[[col]] <- dt[[col]]/dt$denom
    dt$denom <- NULL
    dt <- noStatsFacet(dt, group, panel)
    names(dt) <- c(group, panel, 'label', 'x', 'y')
  }

  return(dt)
}

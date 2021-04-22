binSize <- function(data, col, overlay = NULL, panel = NULL, binWidth = NULL, viewport) {
  aggStr <- getAggStr(col, c('binLabel', 'binStart', 'binEnd', overlay, panel))

  data$binLabel <- bin(data[[col]], binWidth, viewport)
  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)

  dt <- aggregate(as.formula(aggStr), data, length)
  dt <- dt[order(dt$binStart),]
  dt$binStart <- as.character(dt$binStart)
  dt <- noStatsFacet(dt, overlay, panel)
  names(dt) <- c(overlay, panel, 'binLabel', 'binStart', 'binEnd', 'value')

  return(dt)
}

binProportion <- function(data, col, overlay = NULL, panel = NULL, binWidth = NULL, viewport) {
  aggStr <- getAggStr(col, c('binLabel', 'binStart', 'binEnd', overlay, panel))
  aggStr2 <- getAggStr(col, c(overlay, panel))

  data$binLabel <- bin(data[[col]], binWidth, viewport)
  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)

  dt <- aggregate(as.formula(aggStr), data, length)
  dt <- dt[order(dt$binStart),]
  dt$binStart <- as.character(dt$binStart)
  if (is.null(overlay) && is.null(panel)) {
    dt$denom <- length(data[[col]])
    dt <- data.table::data.table('binLabel' = list(dt$binLabel), 'binStart' = list(dt$binStart), 'binEnd' = list(dt$binEnd), 'value' = list(dt[[col]]/dt$denom))
  } else {
    dt2 <- aggregate(as.formula(aggStr2), data, length)
    names(dt2) <- c(overlay, panel, 'denom')
    mergeByCols <- c(overlay, panel)
    dt <- merge(dt, dt2, by = mergeByCols)
    dt[[col]] <- dt[[col]]/dt$denom
    dt$denom <- NULL
    dt <- noStatsFacet(dt, overlay, panel)
    names(dt) <- c(overlay, panel, 'binLabel', 'binStart', 'binEnd', 'value')
  }

  return(dt)
}

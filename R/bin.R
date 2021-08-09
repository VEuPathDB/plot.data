binSize <- function(data, col, group = NULL, panel = NULL, binWidth = NULL, viewport) {
  data <- data[data[[col]] >= viewport$xMin & data[[col]] <= viewport$xMax,]
  data$binLabel <- bin(data[[col]], binWidth, viewport)

  byCols <- colnames(data)[colnames(data) %in% c('binLabel', group, panel)]
  data <- data[, list(value=length(get(..col))), by=eval(byCols)]

  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)
  data <- data[order(data$binStart),]
  data$binStart <- as.character(data$binStart)

  data <- collapseByGroup(data, group, panel)

  return(data)
}

binProportion <- function(data, col, group = NULL, panel = NULL, binWidth = NULL, barmode = 'stack', viewport) {
  data <- data[data[[col]] >= viewport$xMin & data[[col]] <= viewport$xMax,]
  data$binLabel <- bin(data[[col]], binWidth, viewport)
#possible if stacked w no group or panel, we work like overlay?

  # byCols determine the denominator of the proportion calculation
  numCols <- colnames(data)[colnames(data) %in% c('binLabel', group, panel)]
  if (barmode == 'overlay' || all(is.null(c(group,panel)))) {
    denomCols <- colnames(data)[colnames(data) %in% c(group, panel)]
  } else if (barmode == 'stack') {
    denomCols <- colnames(data)[colnames(data) %in% c('binLabel', panel)]
  } else {
    stop('Options for barmode are "stack" or "overlay".')
  }

  denom <- data[, list(sum=length(get(..col))), by=eval(denomCols)]
  if (!length(denomCols)) {
    data$sum <- denom$sum
  } else {
    data <- merge(denom, data, by=eval(denomCols))
  }
  data <- data[, list(value=length(get(..col))/sum), by=eval(numCols)]

  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)
  data <- data[order(data$binStart),]
  data$binStart <- as.character(data$binStart)

  data <- unique(data)
  data <- collapseByGroup(data, group, panel)

  return(data)
}

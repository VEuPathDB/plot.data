binMedian <- function(data, x, y, group = NULL, panel = NULL, binWidth = NULL, viewport, errorBars = c(TRUE, FALSE), xType) {
  errorBars <- veupathUtils::matchArg(errorBars)
  
  if (xType != 'STRING' && binWidth != 0) {
    data <- data[data[[x]] >= viewport$xMin & data[[x]] <= viewport$xMax,]
    data$binLabel <- bin(data[[x]], binWidth, viewport)
  } else {
    data.table::setnames(data, x, 'binLabel')
  }

  byCols <- colnames(data)[colnames(data) %in% c('binLabel', group, panel)]
  if (errorBars) {
    data <- data[, list(value=roundedMedian(get(..y)),
                      binSampleSize=simpleSampleSize(get(..y)),
                      errorBars=medianCI(get(..y))), by=eval(byCols)]
  } else {
    data <- data[, list(value=roundedMedian(get(..y)),
                      binSampleSize=simpleSampleSize(get(..y))), by=eval(byCols)]
  }
  
  if (xType != 'STRING' && binWidth != 0) {
    data$binStart <- findBinStart(data$binLabel)
    data$binEnd <- findBinEnd(data$binLabel)
    data <- data[order(data$binStart),]
    data$binStart <- as.character(data$binStart)
  } else {
    data <- data[order(data$binLabel),]
  }
  data$binLabel <- as.character(data$binLabel)

  data <- collapseByGroup(data, group, panel)

  return(data)
}

binMean <- function(data, x, y, group = NULL, panel = NULL, binWidth = NULL, viewport, errorBars = c(TRUE, FALSE), xType) {
  errorBars <- veupathUtils::matchArg(errorBars)

  if (xType != 'STRING' && binWidth != 0) {
    data <- data[data[[x]] >= viewport$xMin & data[[x]] <= viewport$xMax,]
    data$binLabel <- bin(data[[x]], binWidth, viewport)
  } else {
    data.table::setnames(data, x, 'binLabel')
  }
  
  byCols <- colnames(data)[colnames(data) %in% c('binLabel', group, panel)]
  if (errorBars) {
    data <- data[, list(value=roundedMean(get(..y)),
                      binSampleSize=simpleSampleSize(get(..y)),
                      errorBars=meanCI(get(..y))), by=eval(byCols)]
  } else {
    data <- data[, list(value=roundedMean(get(..y)),
                      binSampleSize=simpleSampleSize(get(..y))), by=eval(byCols)]
  }
  
  if (xType != 'STRING' && binWidth != 0) {
    data$binStart <- findBinStart(data$binLabel)
    data$binEnd <- findBinEnd(data$binLabel)
    data <- data[order(data$binStart),]
    data$binStart <- as.character(data$binStart)
  } else {
    data <- data[order(data$binLabel),]
  }
  data$binLabel <- as.character(data$binLabel)

  data <- collapseByGroup(data, group, panel)

  return(data)
}

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

# essentially compares size of *groups* across bins
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

# finds specific ratios of values/ categories for y by bins+groups
binCategoryProportion <- function(data, x, y, group = NULL, panel = NULL, binWidth = NULL, viewport, errorBars = c(TRUE, FALSE), numeratorValues, denominatorValues, xType) {
  errorBars <- veupathUtils::matchArg(errorBars)

  # TODO improve this as part of #127. shouldnt need an xType arg ideally, just a boolean bin arg.
  if (xType != 'STRING' && binWidth != 0) {
    data <- data[data[[x]] >= viewport$xMin & data[[x]] <= viewport$xMax,]
    data$binLabel <- bin(data[[x]], binWidth, viewport)
  } else {
    data.table::setnames(data, x, 'binLabel')
  }
  
  byCols <- colnames(data)[colnames(data) %in% c('binLabel', group, panel)]
  if (errorBars) {
    data <- data[, { numeratorCount = sum(get(..y) %in% numeratorValues); 
                     denominatorCount = sum(get(..y) %in% denominatorValues);
                     list(value=roundedRatio(numeratorCount, denominatorCount),
                          binSampleSize=formatProportionSampleSize(numeratorCount, denominatorCount),
                          errorBars=proportionCI(numeratorCount, denominatorCount))}, 
                     by=eval(byCols)]
  } else {
    data <- data[, { numeratorCount = sum(get(..y) %in% numeratorValues);
                     denominatorCount = sum(get(..y) %in% denominatorValues);
                     list(value=roundedRatio(numeratorCount, denominatorCount),
                          binSampleSize=formatProportionSampleSize(numeratorCount, denominatorCount))}, 
                     by=eval(byCols)]
  }

  if (xType != 'STRING' && binWidth != 0) {
    data$binStart <- findBinStart(data$binLabel)
    data$binEnd <- findBinEnd(data$binLabel)
    data <- data[order(data$binStart),]
    data$binStart <- as.character(data$binStart)
  } else {
    data <- data[order(data$binLabel),]
  }
  data$binLabel <- as.character(data$binLabel)

  data <- collapseByGroup(data, group, panel)

  return(data)
}

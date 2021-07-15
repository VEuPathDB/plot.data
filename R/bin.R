binSize <- function(data, col, group = NULL, panel = NULL, binWidth = NULL, viewport) {
  aggStr <- getAggStr(col, c('binLabel', 'binStart', 'binEnd', group, panel))

  data <- data[data[[col]] >= viewport$xMin & data[[col]] <= viewport$xMax,]
  data$binLabel <- bin(data[[col]], binWidth, viewport)
  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)

  dt <- aggregate(as.formula(aggStr), data, length)
  dt <- dt[order(dt$binStart),]
  dt$binStart <- as.character(dt$binStart)
  dt <- collapseByGroup(dt, group, panel)
  data.table::setnames(dt, c(group, panel, 'binLabel', 'binStart', 'binEnd', 'value'))

  return(dt)
}

binProportion <- function(data, col, group = NULL, panel = NULL, binWidth = NULL, barmode = 'stack', viewport) {
  aggStr <- getAggStr(col, c('binLabel', 'binStart', 'binEnd', group, panel))


  data <- data[data[[col]] >= viewport$xMin & data[[col]] <= viewport$xMax,]
  data$binLabel <- bin(data[[col]], binWidth, viewport)
  data$binStart <- findBinStart(data$binLabel)
  data$binEnd <- findBinEnd(data$binLabel)

  dt <- aggregate(as.formula(aggStr), data, length)
  dt <- dt[order(dt$binStart),]
  dt$binStart <- as.character(dt$binStart)
  if (is.null(group) && is.null(panel)) {
    dt$denom <- length(data[[col]])
    dt <- data.table::data.table('binLabel' = list(dt$binLabel), 'binStart' = list(dt$binStart), 'binEnd' = list(dt$binEnd), 'value' = list(dt[[col]]/dt$denom))
  } else {
    
    if (barmode == 'overlay') {
      
      # byCols <- c(group, panel)
      
      aggStr2 <- getAggStr(col, c(group, panel))
      dt2 <- aggregate(as.formula(aggStr2), data, length)
      data.table::setnames(dt2, c(group, panel, 'denom'))
      mergeByCols <- c(group, panel)
      dt <- merge(dt, dt2, by = mergeByCols)
      
    } else if (barmode == 'stack') {
      byCols <- c('binLabel', panel)
    
      # if (is.null(panel)) {
      #   
      #   dt$denom <- length(data[[col]])
      #   
      # } else {
        # binLabels act as x values
    
      aggStr2 <- getAggStr(col, c('binLabel', panel))
      dt2 <- aggregate(as.formula(aggStr2), data, length)
      data.table::setnames(dt2, c('binLabel', panel, 'denom'))
      mergeByCols <- c('binLabel', panel)
      dt <- merge(dt, dt2, by = mergeByCols)
      data.table::setcolorder(dt, c(group, colnames(dt)[!(colnames(dt) %in% c(group))]))
        
      # }
    } else {
      stop('Options for barmode are "stack" or "overlay".')
    }
    # dt <- data.table::as.data.table(dt)
    # dt[, sum := sum(get(col)), by=byCols][, value := get(col)/sum]
    # 
    # # Remove unnecessary columns
    # dt[, sum := NULL]
    # dt[, (col) := NULL]
    # data.table::setcolorder(dt, c(group, colnames(dt)[!(colnames(dt) %in% c(group))]))
    dt[[col]] <- dt[[col]]/dt$denom
    dt$denom <- NULL
    dt <- collapseByGroup(dt, group, panel)
    data.table::setnames(dt, c(group, panel, 'binLabel', 'binStart', 'binEnd', 'value'))
  }

  return(dt)
}

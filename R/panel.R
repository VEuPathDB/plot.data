#very specifically not setting these names by ref
#since the new col names 'x' 'y' arent meaningful

panelAllStats <- function(data, x, y, panel = NULL, columnReferenceValue = NA_character_, rowReferenceValue = NA_character_) {
  names(data)[names(data) == x] <- 'x'
  names(data)[names(data) == y] <- 'y'
  x <- 'x'
  y <- 'y'

  nUniqueX <- uniqueN(data[[x]])
  nUniqueY <- uniqueN(data[[y]])

  # If both x and y only have two unique values, then we have a 2x2 table, and we should compute
  # a lot of stats. Otherwise, it's an RxC
  if (nUniqueX <= 2 && nUniqueY <= 2) {
    # 2x2 Stats
    if (is.null(panel)) {
      tbl <- tableXY(data)
      tbl <- TwoByTwoTable('data'=tbl, 'columnReferenceValue'=columnReferenceValue, 'rowReferenceValue'=rowReferenceValue)
      statistics <- allStats(tbl)
      dt <- veupathUtils::as.data.table(statistics)
    } else {
      buildTwoByTwo <- function(tbl) {
        TwoByTwoTable('data' = tbl, 'columnReferenceValue' = columnReferenceValue, 'rowReferenceValue' = rowReferenceValue)
      }

      dt.list <- split(data, list(data[[panel]]))
      dt.list <- lapply(dt.list, tableXY)
      dt.list <- lapply(dt.list, buildTwoByTwo)
      dt.list <- lapply(dt.list, allStats)
      dt.list <- lapply(dt.list, veupathUtils::as.data.table)
      colNames <- names(dt.list[[1]])
      dt <- data.table::as.data.table(lapply(as.list(colNames), function(name) { lapply( dt.list, function(x) {x[[name]]} ) } ))
      data.table::setnames(dt, colNames)
      #dt <- purrr::reduce(dt.list, rbind)
      dt[[panel]] <- names(dt.list)
    }
  } else {
    # RxC Stats. For now just chi squared.
    if (is.null(panel)) {
      tbl <- tableXY(data)
      tbl <- ContingencyTable('data'=tbl, 'columnReferenceValue'=columnReferenceValue, 'rowReferenceValue'=rowReferenceValue)
      statistics <- allStats(tbl)
      dt <- veupathUtils::as.data.table(statistics)
    } else {
      buildContingency <- function(tbl) {
        ContingencyTable('data' = tbl, 'columnReferenceValue' = columnReferenceValue, 'rowReferenceValue' = rowReferenceValue)
      }

      dt.list <- split(data, list(data[[panel]]))
      dt.list <- lapply(dt.list, tableXY)
      dt.list <- lapply(dt.list, buildContingency)
      dt.list <- lapply(dt.list, allStats)
      dt.list <- lapply(dt.list, veupathUtils::as.data.table)
      colNames <- names(dt.list[[1]])
      dt <- data.table::as.data.table(lapply(as.list(colNames), function(name) { lapply( dt.list, function(x) {x[[name]]} ) } ))
      data.table::setnames(dt, colNames)
      dt[[panel]] <- names(dt.list)
      # dt.list <- split(data, list(data[[panel]]))
      # dt.list <- lapply(dt.list, tableXY)
      # dt.list <- lapply(dt.list, function(x) {suppressWarnings(chiSq(x))})
      # dt <- purrr::reduce(dt.list, rbind)
      # dt[[panel]] <- names(dt.list)
    }
  }

  return(dt) 
}

# No longer in use?
panelBothRatios <- function(data, x, y, panel = NULL) {
  names(data)[names(data) == x] <- 'x'
  names(data)[names(data) == y] <- 'y'
  x <- 'x'
  y <- 'y'

  if (is.null(panel)) {
    tbl <- tableXY(data)
    dt <- bothRatios(tbl)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableXY)
    dt.list <- lapply(dt.list, bothRatios)
    dt <- purrr::reduce(dt.list, rbind)
    dt[[panel]] <- names(dt.list)
  }

  return(dt)
}

panelChiSq <- function(data, x, y, panel = NULL) {
  names(data)[names(data) == x] <- 'x'
  names(data)[names(data) == y] <- 'y'
  x <- 'x'
  y <- 'y'

  if (is.null(panel)) {
    tbl <- tableXY(data)
    dt <- suppressWarnings(chiSq(tbl))
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableXY)
    dt.list <- lapply(dt.list, function(x) {suppressWarnings(chiSq(x))})
    dt <- purrr::reduce(dt.list, rbind)
    dt[[panel]] <- names(dt.list)
  }

  return(dt)
}

panelTable <- function(data, x, y, panel = NULL) {
  if (is.null(panel)) {
    dt <- tableAsDT(data, x, y)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableAsDT, x, y)
    dt <- purrr::reduce(dt.list, rbind)
    dt[[panel]] <- names(dt.list)
  }

  return(dt)
}

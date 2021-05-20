#very specifically not setting these names by ref
#since the new col names 'x' 'y' arent meaningful
panelOddsRatio <- function(data, x, y, panel = NULL) {
  names(data)[names(data) == x] <- 'x'
  names(data)[names(data) == y] <- 'y'
  x <- 'x'
  y <- 'y'

  if (is.null(panel)) {
    tbl <- tableXY(data)
    dt <- oddsRatio(tbl)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableXY)
    dt.list <- lapply(dt.list, oddsRatio)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

panelRelativeRisk <- function(data, x, y, panel = NULL) {
  names(data)[names(data) == x] <- 'x'
  names(data)[names(data) == y] <- 'y'
  x <- 'x'
  y <- 'y'

  if (is.null(panel)) {
    tbl <- tableXY(data)
    dt <- relativeRisk(tbl)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableXY)
    dt.list <- lapply(dt.list, relativeRisk)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

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
    dt$panel <- names(dt.list)
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
    dt <- chiSq(tbl)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, tableXY)
    dt.list <- lapply(dt.list, chiSq)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
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
    dt$panel <- names(dt.list)
  }

  return(dt)
}

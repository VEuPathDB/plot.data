panelOddsRatio <- function(data, independent, y, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == y] <- 'y'
  independent <- 'independent'
  y <- 'y'

  if (is.null(panel)) {
    dt <- oddsRatio(data)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, oddsRatio)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

panelRelativeRisk <- function(data, independent, y, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == y] <- 'y'
  independent <- 'independent'
  y <- 'y'

  if (is.null(panel)) {
    dt <- relativeRisk(data)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, relativeRisk)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

panelBothRatios <- function(data, independent, y, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == y] <- 'y'
  independent <- 'independent'
  y <- 'y'
  mergeByCols <- c(panel, 'p.value', 'independent', 'y')

  if (is.null(panel)) {
    dt <- bothRatios(data)
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, bothRatios)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

panelChiSq <- function(data, independent, y, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == y] <- 'y'
  independent <- 'independent'
  y <- 'y'

  if (is.null(panel)) {
    dt <- chiSq(data)
  } else {
    levelsX <- unique(data[[independent]])
    levelsY <- unique(data[[y]])
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, chiSq)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

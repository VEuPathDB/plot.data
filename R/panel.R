panelOddsRatio <- function(data, independent, dependent, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == dependent] <- 'dependent'
  independent <- 'independent'
  dependent <- 'dependent'

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

panelRelativeRisk <- function(data, independent, dependent, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == dependent] <- 'dependent'
  independent <- 'independent'
  dependent <- 'dependent'

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

panelBothRatios <- function(data, independent, dependent, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == dependent] <- 'dependent'
  independent <- 'independent'
  dependent <- 'dependent'
  mergeByCols <- c(panel, 'p.value', 'independent', 'dependent')

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

panelChiSq <- function(data, independent, dependent, panel = NULL) {
  names(data)[names(data) == independent] <- 'independent'
  names(data)[names(data) == dependent] <- 'dependent'
  independent <- 'independent'
  dependent <- 'dependent'

  if (is.null(panel)) {
    dt <- chiSq(data)
  } else {
    levelsX <- unique(data[[independent]])
    levelsY <- unique(data[[dependent]])
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, chiSq)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

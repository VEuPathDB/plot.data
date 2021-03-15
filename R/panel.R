panelOddsRatio <- function(data, col, group, panel = NULL) {
  names(data)[names(data) == col] <- 'x'
  names(data)[names(data) == group] <- 'y'
  col <- 'x'
  group <- 'y'

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

panelRelativeRisk <- function(data, col, group, panel = NULL) {
  names(data)[names(data) == col] <- 'x'
  names(data)[names(data) == group] <- 'y'
  col <- 'x'
  group <- 'y'

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

#TODO update format of response from the epitools functions
#currently returns two rows, we need one of listed vals
panelBothRatios <- function(data, col, group, panel = NULL) {
  names(data)[names(data) == col] <- 'x'
  names(data)[names(data) == group] <- 'y'
  col <- 'x'
  group <- 'y'
  mergeByCols <- c(panel, 'p.value', 'x', 'y')

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

panelChiSq <- function(data, col, group, panel = NULL) {
  names(data)[names(data) == col] <- 'x'
  names(data)[names(data) == group] <- 'y'
  col <- 'x'
  group <- 'y'

  if (is.null(panel)) {
    dt <- chiSq(data)
  } else {
    levelsX <- unique(data[[x]])
    levelsY <- unique(data[[y]])
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, chiSq)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

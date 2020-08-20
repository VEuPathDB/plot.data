panelOddsRatio <- function(data, col, group, panel = NULL) {
  names(data)[names(data) == col] <- 'y'
  names(data)[names(data) == group] <- 'x'
  col <- 'y'
  group <- 'x'

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
  names(data)[names(data) == col] <- 'y'
  names(data)[names(data) == group] <- 'x'
  col <- 'y'
  group <- 'x'

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



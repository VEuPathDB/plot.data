#TODO consider if x and y need reversing.. 

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



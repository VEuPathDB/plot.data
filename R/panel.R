panelOddsRatio <- function(data, col, group, panel = NULL) {

  if (is.null(panel)) {
    dt <- oddsRatio(data[[group]], data[[col]])
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, oddsRatio)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}

panelRelativeRisk <- function(data, col, group, panel = NULL) {

  if (is.null(panel)) {
    dt <- relativeRisk(data[[group]], data[[col]])
  } else {
    dt.list <- split(data, list(data[[panel]]))
    dt.list <- lapply(dt.list, relativeRisk)
    dt <- purrr::reduce(dt.list, rbind)
    dt$panel <- names(dt.list)
  }

  return(dt)
}



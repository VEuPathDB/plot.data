#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @return data.table plot-ready data
#' @export
mosaic.dt <- function(data, map) {
  group <- emptyStringToNull(map$id[map$plotRef == 'yAxisVariable'])
  x <- emptyStringToNull(map$id[map$plotRef == 'xAxisVariable'])
  facet1 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable1'])
  facet2 <- emptyStringToNull(map$id[map$plotRef == 'facetVariable2'])

  panelData <- makePanels(data, facet1, facet2)
  data <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  data.back <- data
  myCols <- c(x, group, panel)
  data <- data[, myCols, with=FALSE]

  dims <- as.data.frame.matrix(table(data[[x]], data[[group]]))
  dims <- c(length(dims), nrow(dims))

  if (any(dims > 2)) {
    data <- panelChiSq(data, x, group, panel)
  } else {
    data <- panelBothRatios(data, x, group, panel)
  }

   #TODO check this works, since we only have panels here really
  data.back <- noStatsFacet(data.back, NULL, panel)
  data.back <- data.back[, -c(x), with = FALSE]
  if (!is.null(key(data.back))) {
    data <- merge(data, data.back)
  } else {
    data <- cbind(data, data.back)
  }

  return(data)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot
#' @return character name of json file containing plot-ready data
#' @export
mosaic <- function(data, map) {
  dt <- mosaic.dt(data, map)
  outFileName <- writeJSON(data, 'mosaic')

  return(outFileName)
}

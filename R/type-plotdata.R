# TODO consider alternate ways of representing the list args
newPlotdata <- function(.dt = data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         zAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         ...,
                         class = character()) {

  x <- emptyStringToNull(as.character(xAxisVariable$variableId))
  y <- emptyStringToNull(as.character(yAxisVariable$variableId))
  z <- emptyStringToNull(as.character(zAxisVariable$variableId))
  group <- emptyStringToNull(as.character(overlayVariable$variableId))
  facet1 <- emptyStringToNull(as.character(facetVariable1$variableId))
  facet2 <- emptyStringToNull(as.character(facetVariable2$variableId))

  #TODO need to be able to optionally pass y and z axes
  panelData <- makePanels(.dt, facet1, facet2)
  .dt <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  myCols <- c(x, y, z, group, panel)
  .dt <- .dt[, myCols, with=FALSE]

  incompleteCases <- nrow(.dt[!complete.cases(.dt),])
  .dt <- .dt[complete.cases(.dt),]

  if (is.null(xAxisVariable$dataType)) {
    xIsNum = all(!is.na(as.numeric(.dt[[x]])))
    xAxisVariable$dataType <- 'NUMBER'
    xIsDate = !xIsNum && all(!is.na(as.POSIXct(.dt[[x]], format='%Y-%m-%d')))
    xAxisVariable$dataType <- 'DATE'
    xIsChar = !xIsNum && !xIsDate && all(!is.na(as.character(.dt[[x]])))
    xAxisVariable$dataType <- 'STRING'
  } else {
    xIsChar = xAxisVariable$dataType == 'STRING'
    xIsNum = xAxisVariable$dataType == 'NUMBER'
    xIsDate = xAxisVariable$dataType == 'DATE'
  } 

  attr <- attributes(.dt)
  attr$xAxisVariable <-  xAxisVariable
  if (!is.null(y)) { attr$yAxisVariable <- yAxisVariable }
  if (!is.null(z)) { attr$yAxisVariable <- zAxisVariable }
  attr$incompleteCases <- incompleteCases
  attr$class = c(class, 'plot.data', attr$class)
  if (!is.null(group)) { attr$overlayVariable <- overlayVariable }
  if (!is.null(facet1)) { attr$facetVariable1 <- facetVariable1 }
  if (!is.null(facet2)) { attr$facetVariable2 <- facetVariable2 }

  attributes(.dt) <- attr
  .pd <- validatePlotdata(.dt)

  return(.pd)
}

validateVariableAttr <- function(variableAttr) {
  if (!is.list(variableAttr)) {
    return(FALSE)
  } else {
    if (!'variableId' %in% names(variableAttr)) {
      return(FALSE)
    }
  }

  return(TRUE)
}

validatePlotdata <- function(.pd) {
  .dt <- unclass(.pd)
  xAxisVariable <- attr(.pd, 'xAxisVariable')
  stopifnot(validateVariableAttr(xAxisVariable))
  stopifnot(xAxisVariable$variableId %in% names(.dt))
  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

#TODO make a generic & methods for phyloseq, maybe ggplot obj ?
# related, make helper as.plot.data that consumes these too
# think as.plot.data needs to have an arg for plot type
#  this to avoid loads of as.* helpers
# may not need them for alpha div, richness data may be a data.frame
#TODO also consider custom as.data.table that takes a plot.data
#TODO do we need a helper as.plot.data for data.table ?

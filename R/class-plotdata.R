newPlotdata <- function(.dt = data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         zAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL),
                         ...,
                         class = character()) {

  x <- emptyStringToNull(as.character(xAxisVariable$variableId))
  xType <- emptyStringToNull(as.character(xAxisVariable$dataType))
  y <- emptyStringToNull(as.character(yAxisVariable$variableId))
  yType <- emptyStringToNull(as.character(yAxisVariable$dataType))
  z <- emptyStringToNull(as.character(zAxisVariable$variableId))
  zType <- emptyStringToNull(as.character(zAxisVariable$dataType))
  group <- emptyStringToNull(as.character(overlayVariable$variableId))
  groupType <- emptyStringToNull(as.character(overlayVariable$dataType))
  facet1 <- emptyStringToNull(as.character(facetVariable1$variableId))
  facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
  facet2 <- emptyStringToNull(as.character(facetVariable2$variableId))
  facetType2 <- emptyStringToNull(as.character(facetVariable2$dataType))

  .dt[[x]] <- updateType(.dt[[x]], xType)
  if (!is.null(y)) { .dt[[y]] <- updateType(.dt[[y]], yType) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType) }
  if (!is.null(group)) { .dt[[group]] <- updateType(.dt[[group]], groupType) }
  if (!is.null(facet1)) { .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1) }
  if (!is.null(facet2)) { .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2) }

  panelData <- makePanels(.dt, facet1, facet2)
  .dt <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  myCols <- c(x, y, z, group, panel)
  .dt <- .dt[, myCols, with=FALSE]

  incompleteCases <- jsonlite::unbox(nrow(.dt[!complete.cases(.dt),]))
  .dt <- .dt[complete.cases(.dt),]

  if (xType == 'STRING') {
    .dt$dummy <- 1
    sampleSizeTable <- groupSize(.dt, x=x, y="dummy", group, panel, collapse=F)
    .dt$dummy <- NULL
  } else {
    sampleSizeTable <- groupSize(.dt, x=NULL, y=x, group, panel, collapse=F)
  }
  sampleSizeTable$size <- lapply(sampleSizeTable$size, jsonlite::unbox)

  if (is.null(xAxisVariable$dataType)) {
    xIsNum = all(!is.na(as.numeric(.dt[[x]])))
    xAxisVariable$dataType <- 'NUMBER'
    xIsDate = !xIsNum && all(!is.na(as.Date(.dt[[x]], format='%Y-%m-%d')))
    xAxisVariable$dataType <- 'DATE'
    xIsChar = !xIsNum && !xIsDate && all(!is.na(as.character(.dt[[x]])))
    xAxisVariable$dataType <- 'STRING'
  } 

  attr <- attributes(.dt)
  attr$xAxisVariable <-  xAxisVariable
  if (!is.null(y)) { attr$yAxisVariable <- yAxisVariable }
  if (!is.null(z)) { attr$yAxisVariable <- zAxisVariable }
  attr$incompleteCases <- incompleteCases
  attr$sampleSizeTable <- collapseByGroup(sampleSizeTable, group, panel)
  attr$class = c(class, 'plot.data', attr$class)
  if (!is.null(group)) { attr$overlayVariable <- overlayVariable }
  if (!is.null(facet1)) { attr$facetVariable1 <- facetVariable1 }
  if (!is.null(facet2)) { attr$facetVariable2 <- facetVariable2 }

  setAttrFromList(.dt, attr)
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

sampleSizeTable <- function(.pd) { attr(.pd, 'sampleSizeTable') }

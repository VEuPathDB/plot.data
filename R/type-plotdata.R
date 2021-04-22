newPlotdata <- function(.dt = data.table(),
                         independentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         dependentVar = list('variableId' = NULL,
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

  independent <- emptyStringToNull(as.character(independentVar$variableId))
  xType <- emptyStringToNull(as.character(independentVar$dataType))
  dependent <- emptyStringToNull(as.character(dependentVar$variableId))
  yType <- emptyStringToNull(as.character(dependentVar$dataType))
  z <- emptyStringToNull(as.character(zAxisVariable$variableId))
  zType <- emptyStringToNull(as.character(zAxisVariable$dataType))
  group <- emptyStringToNull(as.character(overlayVariable$variableId))
  groupType <- emptyStringToNull(as.character(overlayVariable$dataType))
  facet1 <- emptyStringToNull(as.character(facetVariable1$variableId))
  facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
  facet2 <- emptyStringToNull(as.character(facetVariable2$variableId))
  facetType2 <- emptyStringToNull(as.character(facetVariable2$dataType))

  .dt[[independent]] <- updateType(.dt[[independent]], xType)
  if (!is.null(dependent)) { .dt[[dependent]] <- updateType(.dt[[dependent]], yType) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType) }
  if (!is.null(group)) { .dt[[group]] <- updateType(.dt[[group]], groupType) }
  if (!is.null(facet1)) { .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1) }
  if (!is.null(facet2)) { .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2) }

  panelData <- makePanels(.dt, facet1, facet2)
  .dt <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  myCols <- c(independent, dependent, z, group, panel)
  .dt <- .dt[, myCols, with=FALSE]

  incompleteCases <- nrow(.dt[!complete.cases(.dt),])
  .dt <- .dt[complete.cases(.dt),]

  if (is.null(independentVar$dataType)) {
    xIsNum = all(!is.na(as.numeric(.dt[[independent]])))
    independentVar$dataType <- 'NUMBER'
    xIsDate = !xIsNum && all(!is.na(as.Date(.dt[[independent]], format='%Y-%m-%d')))
    independentVar$dataType <- 'DATE'
    xIsChar = !xIsNum && !xIsDate && all(!is.na(as.character(.dt[[independent]])))
    independentVar$dataType <- 'STRING'
  } else {
    xIsChar = independentVar$dataType == 'STRING'
    xIsNum = independentVar$dataType == 'NUMBER'
    xIsDate = independentVar$dataType == 'DATE'
  } 

  attr <- attributes(.dt)
  attr$independentVar <-  independentVar
  if (!is.null(dependent)) { attr$dependentVar <- dependentVar }
  if (!is.null(z)) { attr$dependentVar <- zAxisVariable }
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
  independentVar <- attr(.pd, 'independentVar')
  stopifnot(validateVariableAttr(independentVar))
  stopifnot(independentVar$variableId %in% names(.dt))
  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

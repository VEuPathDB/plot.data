newPlotdata <- function(.dt = data.table(),
                         independentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         dependentVar = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL),
                         gradientVar = list('variableId' = NULL,
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
  independentType <- emptyStringToNull(as.character(independentVar$dataType))
  dependent <- emptyStringToNull(as.character(dependentVar$variableId))
  dependentType <- emptyStringToNull(as.character(dependentVar$dataType))
  gradient <- emptyStringToNull(as.character(gradientVar$variableId))
  gradientType <- emptyStringToNull(as.character(gradientVar$dataType))
  overlay <- emptyStringToNull(as.character(overlayVariable$variableId))
  overlayType <- emptyStringToNull(as.character(overlayVariable$dataType))
  facet1 <- emptyStringToNull(as.character(facetVariable1$variableId))
  facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
  facet2 <- emptyStringToNull(as.character(facetVariable2$variableId))
  facetType2 <- emptyStringToNull(as.character(facetVariable2$dataType))

  .dt[[independent]] <- updateType(.dt[[independent]], independentType)
  if (!is.null(dependent)) { .dt[[dependent]] <- updateType(.dt[[dependent]], dependentType) }
  if (!is.null(gradient)) { .dt[[gradient]] <- updateType(.dt[[gradient]], gradientType) }
  if (!is.null(overlay)) { .dt[[overlay]] <- updateType(.dt[[overlay]], overlayType) }
  if (!is.null(facet1)) { .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1) }
  if (!is.null(facet2)) { .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2) }

  panelData <- makePanels(.dt, facet1, facet2)
  .dt <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  myCols <- c(independent, dependent, gradient, overlay, panel)
  .dt <- .dt[, myCols, with=FALSE]

  incompleteCases <- nrow(.dt[!complete.cases(.dt),])
  .dt <- .dt[complete.cases(.dt),]

  if (is.null(independentVar$dataType)) {
    independentIsNum = all(!is.na(as.numeric(.dt[[independent]])))
    independentVar$dataType <- 'NUMBER'
    independentIsDate = !independentIsNum && all(!is.na(as.Date(.dt[[independent]], format='%Y-%m-%d')))
    independentVar$dataType <- 'DATE'
    independentIsChar = !independentIsNum && !independentIsDate && all(!is.na(as.character(.dt[[independent]])))
    independentVar$dataType <- 'STRING'
  } else {
    independentIsChar = independentVar$dataType == 'STRING'
    independentIsNum = independentVar$dataType == 'NUMBER'
    independentIsDate = independentVar$dataType == 'DATE'
  } 

  attr <- attributes(.dt)
  attr$independentVar <-  independentVar
  if (!is.null(dependent)) { attr$dependentVar <- dependentVar }
  if (!is.null(gradient)) { attr$dependentVar <- gradientVar }
  attr$incompleteCases <- incompleteCases
  attr$class = c(class, 'plot.data', attr$class)
  if (!is.null(overlay)) { attr$overlayVariable <- overlayVariable }
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

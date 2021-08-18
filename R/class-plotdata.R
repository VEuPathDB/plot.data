### evilMode will do the following:
###   - return 'no data' as a regular value for strata vars but will discard such cases for the axes vars
###   - not return statsTables
###   - allow smoothed means and agg values etc over axes values where we have no data for the strata vars
###   - return a total count of plotted incomplete cases
###   - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul

newPlotdata <- function(.dt = data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         zAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         evilMode = logical(),
                         listVarDetails = list('listValueVariable' = NULL,
                                               'listValuePlotRef' = NULL,
                                               'listVarPlotRef' = NULL,
                                               'listVarDisplayLabel' = NULL),
                         ...,
                         class = character()) {

  x <- toColNameOrNull(xAxisVariable)
  xType <- emptyStringToNull(as.character(xAxisVariable$dataType))
  xShape <- emptyStringToNull(as.character(xAxisVariable$dataShape))
  y <- toColNameOrNull(yAxisVariable)
  yType <- emptyStringToNull(as.character(yAxisVariable$dataType))
  yShape <- emptyStringToNull(as.character(yAxisVariable$dataShape))
  z <- toColNameOrNull(zAxisVariable)
  zType <- emptyStringToNull(as.character(zAxisVariable$dataType))
  zShape <- emptyStringToNull(as.character(zAxisVariable$dataShape))
  group <- toColNameOrNull(overlayVariable)
  groupType <- emptyStringToNull(as.character(overlayVariable$dataType))
  groupShape <- emptyStringToNull(as.character(overlayVariable$dataShape))
  facet1 <- toColNameOrNull(facetVariable1)
  facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
  facetShape1 <- emptyStringToNull(as.character(facetVariable1$dataShape))
  facet2 <- toColNameOrNull(facetVariable2)
  facetType2 <- emptyStringToNull(as.character(facetVariable2$dataType))
  facetShape2 <- emptyStringToNull(as.character(facetVariable2$dataShape))

  if (length(x) == 1) {
    .dt[[x]] <- updateType(.dt[[x]], xType, xShape)
  } else {
    xVars <- lapply(.dt[, ..x], updateType, unique(xType), unique(xShape))
    .dt[, (x):=xVars]
  }
  if (!is.null(y)) { .dt[[y]] <- updateType(.dt[[y]], yType, yShape) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType, zShape) }
  if (!is.null(group)) {
    if (length(group) == 1) {
      .dt[[group]] <- updateType(.dt[[group]], groupType, groupShape)
    } else {
      groupVars <- lapply(.dt[, ..group], updateType, unique(groupType), unique(groupShape))
      .dt[, (group):=groupVars]
    }
  }
  if (!is.null(facet1)) { 
    if (length(facet1)==1) {
      .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1, facetShape1)
    } else {
      facet1Vars <- lapply(.dt[, ..facet1], updateType, unique(facetType1), unique(facetShape1))
      .dt[, (facet1):=facet1Vars]
    }
  }
  if (!is.null(facet2)) { .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2, facetShape2) }

  varCols <- c(x, y, z, group, facet1, facet2)
  completeCasesTable <- data.table::setDT(lapply(.dt[, ..varCols], function(a) {sum(complete.cases(a))}))
  completeCasesTable <- data.table::transpose(completeCasesTable, keep.names = 'variableDetails')
  data.table::setnames(completeCasesTable, 'V1', 'completeCases')
  
  panelData <- makePanels(.dt, facet1, facet2)
  .dt <- data.table::setDT(panelData[[1]])
  panel <- panelData[[2]]
  myCols <- c(x, y, z, group, panel)
  .dt <- .dt[, myCols, with=FALSE]

  completeCases <- jsonlite::unbox(nrow(.dt[complete.cases(.dt),]))
  #### If we have a listvar, since everything gets directed to y we should just skip this step?
  if (evilMode) {
    if (!is.null(group)) { .dt[[group]][is.na(.dt[[group]])] <- 'No data' }
    if (!is.null(panel)) { .dt[[panel]][is.na(.dt[[panel]])] <- 'No data' }
    axesCols <- c(x, y, z)
    axesDT <- .dt[, axesCols, with = FALSE]
    .dt <- .dt[complete.cases(axesDT)]
  } else { 
    .dt <- .dt[complete.cases(.dt),]
  }
  plottedIncompleteCases <- jsonlite::unbox(nrow(.dt[complete.cases(.dt),]) - completeCases)

  #### Handle listvar
  listVariable <- NULL
  if (!is.null(listVarDetails$listVarPlotRef)) {

    if (listVarDetails$listVarPlotRef == 'xAxisVariable') { listVariable <- xAxisVariable
    } else if (listVarDetails$listVarPlotRef == 'overlayVariable') { listVariable <- overlayVariable
    } else if (listVarDetails$listVarPlotRef == 'facetVariable1') {listVariable <- facetVariable1
    } else { stop('listVar error: unaccepted value passed as listVarPlotRef')}
    listValue <- listVarDetails$listValueVariable

    # Validation
    if (is.null(listVarDetails$listValueVariable$variableId)) stop("listVar error: listValue variableId must not be NULL")
    listVariable <- validateListVar(listVariable)

    # Set variable, value names appropriately
    if(is.null(unique(listVarDetails$listValueVariable$entityId))) {
      variable.name <- listVarDetails$listVarPlotRef
      value.name <- listVarDetails$listValueVariable$variableId
    } else {
      variable.name <- paste(unique(listVarDetails$listValueVariable$entityId),listVarDetails$listVarPlotRef, sep='.')
      value.name <- paste(unique(listVarDetails$listValueVariable$entityId),listVarDetails$listValueVariable$variableId, sep='.')
    }

    .dt <- data.table::melt(.dt, measure.vars = toColNameOrNull(listVariable),
                        variable.factor = FALSE,
                        variable.name= variable.name,
                        value.name=value.name)

    #### assign new variable details
    newVariable <- list('variableId' = listVarDetails$listVarPlotRef,
                   'entityId' = unique(listVarDetails$listValueVariable$entityId),
                   'dataType' = 'STRING',
                   'dataShape' = 'CATEGORICAL',
                   'displayLabel' = listVarDetails$listVarDisplayLabel)

    if (listVarDetails$listVarPlotRef == 'xAxisVariable') {
      xAxisVariable <- newVariable
      x <- toColNameOrNull(xAxisVariable)
      xType <- emptyStringToNull(as.character(xAxisVariable$dataType))
      xShape <- emptyStringToNull(as.character(xAxisVariable$dataShape))
      .dt[[x]] <- updateType(.dt[[x]], xType, xShape)

    } else if (listVarDetails$listVarPlotRef == 'overlayVariable') {
      overlayVariable <- newVariable
      group <- toColNameOrNull(overlayVariable)
      groupType <- emptyStringToNull(as.character(overlayVariable$dataType))
      groupShape <- emptyStringToNull(as.character(overlayVariable$dataShape))
      .dt[[group]] <- updateType(.dt[[group]], groupType, groupShape)

    } else if (listVarDetails$listVarPlotRef == 'facetVariable1') {
      facetVariable1 <- newVariable
      facet1 <- toColNameOrNull(facetVariable1)
      facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
      facetShape1 <- emptyStringToNull(as.character(facetVariable1$dataShape))
      .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1, facetShape1) 

      #### Possibly overkill. If we only allow 1 facet just rename panel
      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]
    }

    # Update y
    yAxisVariable <- listVarDetails$listValueVariable
    y <- toColNameOrNull(yAxisVariable)
    yType <- emptyStringToNull(as.character(yAxisVariable$dataType))
    yShape <- emptyStringToNull(as.character(yAxisVariable$dataShape))
    .dt[[y]] <- updateType(.dt[[y]], yType, yShape) 

    data.table::setcolorder(.dt, c(x, y, z, group, panel))

  }
  
  # If overlay is continuous and NOT a listvar, it does not contribute to final groups
  overlayGroup <- if (identical(overlayVariable$dataShape,'CONTINUOUS')) NULL else group

  if (xShape != 'CONTINUOUS') {
    .dt$dummy <- 1
    sampleSizeTable <- groupSize(.dt, x=x, y="dummy", overlayGroup, panel, collapse=F)
    .dt$dummy <- NULL
  } else {
    sampleSizeTable <- groupSize(.dt, x=NULL, y=x, overlayGroup, panel, collapse=F)
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
  attr$completeCases <- completeCases
  attr$plottedIncompleteCases <- plottedIncompleteCases
  attr$completeCasesTable <- completeCasesTable
  attr$sampleSizeTable <- collapseByGroup(sampleSizeTable, overlayGroup, panel)
  attr$class = c(class, 'plot.data', attr$class)
  if (!is.null(group)) { attr$overlayVariable <- overlayVariable }
  if (!is.null(facet1)) { attr$facetVariable1 <- facetVariable1 }
  if (!is.null(facet2)) { attr$facetVariable2 <- facetVariable2 }
  if (!is.null(listVariable)) { attr$listVariable <- listVariable }

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
  stopifnot(toColNameOrNull(xAxisVariable) %in% names(.dt))
  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

# Additional accessor functions
sampleSizeTable <- function(.pd) { attr(.pd, 'sampleSizeTable') }
completeCasesTable <- function(.pd) { attr(.pd, 'completeCasesTable')}
#these helpers need either validation or to be a dedicated method
statsTable <- function(.pd) { attr(.pd, 'statsTable') }

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
                         listVarDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'listVarPlotRef' = NULL,
                                               'listVarDisplayLabel' = NULL),
                         verbose = logical(),
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


  varCols <- c(x, y, z, group, facet1, facet2)
  completeCasesTable <- data.table::setDT(lapply(.dt[, ..varCols], function(a) {sum(complete.cases(a))}))
  completeCasesTable <- data.table::transpose(completeCasesTable, keep.names = 'variableDetails')
  data.table::setnames(completeCasesTable, 'V1', 'completeCases')
  
  logWithTime('Determined the number of complete cases per variable.', verbose)
  
  if (!identical(listVarDetails$listVarPlotRef, 'facetVariable1') & !identical(listVarDetails$listVarPlotRef, 'facetVariable2')) {
    panelData <- makePanels(.dt, facet1, facet2)
    .dt <- data.table::setDT(panelData[[1]])
    panel <- panelData[[2]]
    if (!is.null(panel)){
      if (uniqueN(.dt[[panel]]) > 25) stop("Maximum number of panels allowed is 25.")
    }
  } else {
    panel <- c(facet1, facet2)
  }

  myCols <- c(x, y, z, group, panel)
  .dt <- .dt[, myCols, with=FALSE]
  logWithTime('Identified facet intersections.', verbose)

  # Reshape data and remap variables if listVar is specified
  listVariable <- NULL
  if (!is.null(listVarDetails$listVarPlotRef)) {

    if (listVarDetails$listVarPlotRef == 'xAxisVariable') { listVariable <- xAxisVariable
    } else if (listVarDetails$listVarPlotRef == 'overlayVariable') { listVariable <- overlayVariable
    } else if (listVarDetails$listVarPlotRef == 'facetVariable1') {listVariable <- facetVariable1
    } else if (listVarDetails$listVarPlotRef == 'facetVariable2') {listVariable <- facetVariable2
    } else { stop('listVar error: unaccepted value passed as listVarPlotRef')}
    listValue <- listVarDetails$inferredVariable
    logWithTime('Identified listVariable.', verbose)

    # Validation
    if (is.null(listVarDetails$inferredVariable$variableId)) stop('listVar error: listValue variableId must not be NULL')
    if (listVarDetails$listVarPlotRef != 'xAxisVariable' & evilMode) stop('listVar error: evilMode not compatible.')
    listVariable <- validateListVar(listVariable)
    logWithTime('listVariable has been validated.', verbose)

    # Set variable, value names appropriately
    if(is.null(unique(listVarDetails$inferredVariable$entityId))) {
      variable.name <- listVarDetails$listVarPlotRef
      value.name <- listVarDetails$inferredVariable$variableId
    } else {
      variable.name <- paste(unique(listVarDetails$inferredVariable$entityId),listVarDetails$listVarPlotRef, sep='.')
      value.name <- paste(unique(listVarDetails$inferredVariable$entityId),listVarDetails$inferredVariable$variableId, sep='.')
    }

    # Reshape data
    .dt <- data.table::melt(.dt, measure.vars = toColNameOrNull(listVariable),
                        variable.factor = FALSE,
                        variable.name= variable.name,
                        value.name=value.name)

    logWithTime('Data reshaped according to listVariable.', verbose)

    # Replace listVar values (previously column names) with display labels or variableId
    .dt[[variable.name]] <- lapply(.dt[[variable.name]], toIdOrDisplayLabel, listVariable)

    # Assign new variable details for the created categorical variable
    newCatVariable <- list('variableId' = listVarDetails$listVarPlotRef,
                   'entityId' = unique(listVariable$entityId),
                   'dataType' = 'STRING',
                   'dataShape' = 'CATEGORICAL',
                   'displayLabel' = listVarDetails$listVarDisplayLabel)

    if (listVarDetails$listVarPlotRef == 'xAxisVariable') {
      xAxisVariable <- newCatVariable
      x <- toColNameOrNull(xAxisVariable)
      xType <- emptyStringToNull(as.character(xAxisVariable$dataType))
      xShape <- emptyStringToNull(as.character(xAxisVariable$dataShape))
      .dt[[x]] <- updateType(.dt[[x]], xType, xShape)

    } else if (listVarDetails$listVarPlotRef == 'overlayVariable') {
      overlayVariable <- newCatVariable
      group <- toColNameOrNull(overlayVariable)
      groupType <- emptyStringToNull(as.character(overlayVariable$dataType))
      groupShape <- emptyStringToNull(as.character(overlayVariable$dataShape))
      .dt[[group]] <- updateType(.dt[[group]], groupType, groupShape)

    } else if (listVarDetails$listVarPlotRef == 'facetVariable1') {
      facetVariable1 <- newCatVariable
      facet1 <- toColNameOrNull(facetVariable1)
      facetType1 <- emptyStringToNull(as.character(facetVariable1$dataType))
      facetShape1 <- emptyStringToNull(as.character(facetVariable1$dataShape))
      .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1, facetShape1) 

      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]

    } else if (listVarDetails$listVarPlotRef == 'facetVariable2') {
      facetVariable2 <- newCatVariable
      facet2 <- toColNameOrNull(facetVariable2)
      facetType2 <- emptyStringToNull(as.character(facetVariable2$dataType))
      facetShape2 <- emptyStringToNull(as.character(facetVariable2$dataShape))
      .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2, facetShape2) 

      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]
      if (uniqueN(.dt[[panel]]) > 25) stop("Maximum number of panels allowed is 25.")
    }

    # Assume inferredVarPlotRef = yAxisVariable always.
    yAxisVariable <- listVarDetails$inferredVariable
    y <- toColNameOrNull(yAxisVariable)
    yType <- emptyStringToNull(as.character(yAxisVariable$dataType))
    yShape <- emptyStringToNull(as.character(yAxisVariable$dataShape))
    .dt[[y]] <- updateType(.dt[[y]], yType, yShape) 

    data.table::setcolorder(.dt, c(x, y, z, group, panel))

    logWithTime('Handling of listVariables complete.', verbose)

  }

  # Update types
  .dt[[x]] <- updateType(.dt[[x]], xType, xShape)
  if (!is.null(y)) { .dt[[y]] <- updateType(.dt[[y]], yType, yShape) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType, zShape) }
  if (!is.null(group)) { .dt[[group]] <- updateType(.dt[[group]], groupType, groupShape) }
  if (!is.null(panel)) { .dt[[panel]] <- updateType(.dt[[panel]], 'STRING', 'CATEGORICAL') }
  logWithTime('Base data types updated for all columns as necessary.', verbose)

  completeCasesAllVars <- jsonlite::unbox(nrow(.dt[complete.cases(.dt),]))
  completeCasesAxesVars <- jsonlite::unbox(nrow(.dt[complete.cases(.dt[, c(x,y), with=FALSE]),]))
  logWithTime('Determined total number of complete cases across axes and strata vars.', verbose)

  if (evilMode) {
    if (!is.null(group)) { .dt[[group]][is.na(.dt[[group]])] <- 'No data' }
    if (!is.null(panel)) { .dt[[panel]][is.na(.dt[[panel]])] <- 'No data' }
    axesCols <- c(x, y, z)
    axesDT <- .dt[, axesCols, with = FALSE]
    .dt <- .dt[complete.cases(axesDT)]
  } else { 
    .dt <- .dt[complete.cases(.dt),]
  }

  # If overlay is continuous, it does not contribute to final groups
  overlayGroup <- if (identical(overlayVariable$dataShape,'CONTINUOUS')) NULL else group

  if (xShape != 'CONTINUOUS') {
    .dt$dummy <- 1
    sampleSizeTable <- groupSize(.dt, x=x, y="dummy", overlayGroup, panel, collapse=F)
    .dt$dummy <- NULL
  } else {
    sampleSizeTable <- groupSize(.dt, x=NULL, y=x, overlayGroup, panel, collapse=F)
  }

  logWithTime('Calculated sample sizes per group.', verbose)

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
  attr$completeCasesAllVars <- completeCasesAllVars
  attr$completeCasesAxesVars <- completeCasesAxesVars
  attr$completeCasesTable <- completeCasesTable
  attr$sampleSizeTable <- collapseByGroup(sampleSizeTable, overlayGroup, panel)
  attr$class = c(class, 'plot.data', attr$class)
  if (!is.null(group)) { attr$overlayVariable <- overlayVariable }
  if (!is.null(facet1)) { attr$facetVariable1 <- facetVariable1 }
  if (!is.null(facet2)) { attr$facetVariable2 <- facetVariable2 }
  if (!is.null(listVariable)) { attr$listVariable <- listVariable }

  setAttrFromList(.dt, attr)
  .pd <- validatePlotdata(.dt)
  logWithTime('Base plot.data object created.', verbose)

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

### evilMode will do the following:
###   - return 'no data' as a regular value for strata vars but will discard such cases for the axes vars
###   - not return statsTables
###   - allow smoothed means and agg values etc over axes values where we have no data for the strata vars
###   - return a total count of plotted incomplete cases
###   - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul

#' @importFrom stats complete.cases
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
                         collectionVariableDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'collectionVariablePlotRef' = NULL),
                         computedVariableMetadata = list('displayName' = NULL,
                                                         'displayRangeMin' = NULL,
                                                         'displayRangeMax' = NULL,
                                                         'collectionVariable' = NULL),
                         verbose = logical(),
                         ...,
                         class = character()) {

  x <- veupathUtils::toColNameOrNull(xAxisVariable)
  xType <- veupathUtils::toStringOrNull(as.character(xAxisVariable$dataType))
  xShape <- veupathUtils::toStringOrNull(as.character(xAxisVariable$dataShape))
  y <- veupathUtils::toColNameOrNull(yAxisVariable)
  yType <- veupathUtils::toStringOrNull(as.character(yAxisVariable$dataType))
  z <- veupathUtils::toColNameOrNull(zAxisVariable)
  zType <- veupathUtils::toStringOrNull(as.character(zAxisVariable$dataType))
  group <- veupathUtils::toColNameOrNull(overlayVariable)
  groupType <- veupathUtils::toStringOrNull(as.character(overlayVariable$dataType))
  facet1 <- veupathUtils::toColNameOrNull(facetVariable1)
  facetType1 <- veupathUtils::toStringOrNull(as.character(facetVariable1$dataType))
  facet2 <- veupathUtils::toColNameOrNull(facetVariable2)
  facetType2 <- veupathUtils::toStringOrNull(as.character(facetVariable2$dataType))


  varCols <- c(x, y, z, group, facet1, facet2)
  completeCasesTable <- data.table::setDT(lapply(.dt[, ..varCols], function(a) {sum(complete.cases(a))}))
  completeCasesTable <- data.table::transpose(completeCasesTable, keep.names = 'variableDetails')
  data.table::setnames(completeCasesTable, 'V1', 'completeCases')
  
  veupathUtils::logWithTime('Determined the number of complete cases per variable.', verbose)
  
  if (!identical(collectionVariableDetails$collectionVariablePlotRef, 'facetVariable1') & !identical(collectionVariableDetails$collectionVariablePlotRef, 'facetVariable2')) {
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
  veupathUtils::logWithTime('Identified facet intersections.', verbose)

  # Reshape data and remap variables if collectionVar is specified
  collectionVariable <- NULL
  if (!is.null(collectionVariableDetails$collectionVariablePlotRef)) {

    if (collectionVariableDetails$collectionVariablePlotRef == 'xAxisVariable') { collectionVariable <- xAxisVariable
    } else if (collectionVariableDetails$collectionVariablePlotRef == 'overlayVariable') { collectionVariable <- overlayVariable
    } else if (collectionVariableDetails$collectionVariablePlotRef == 'facetVariable1') {collectionVariable <- facetVariable1
    } else if (collectionVariableDetails$collectionVariablePlotRef == 'facetVariable2') {collectionVariable <- facetVariable2
    } else { stop('collectionVar error: unaccepted value passed as collectionVariablePlotRef')}
    collectionVariable$collectionVariablePlotRef <- collectionVariableDetails$collectionVariablePlotRef
    collectionVariable$collectionValuePlotRef <- 'yAxisVariable'
    listValue <- collectionVariableDetails$inferredVariable
    veupathUtils::logWithTime('Identified collectionVariable.', verbose)

    # Validation
    if (is.null(collectionVariableDetails$inferredVariable$variableId)) stop('collectionVar error: listValue variableId must not be NULL')
    if (collectionVariableDetails$collectionVariablePlotRef != 'xAxisVariable' & evilMode) stop('collectionVar error: evilMode not compatible.')
    collectionVariable <- validatecollectionVar(collectionVariable)
    veupathUtils::logWithTime('collectionVariable has been validated.', verbose)

    # Set variable, value names appropriately
    if(is.null(unique(collectionVariableDetails$inferredVariable$entityId))) {
      variable.name <- collectionVariableDetails$collectionVariablePlotRef
      value.name <- collectionVariableDetails$inferredVariable$variableId
    } else {
      variable.name <- paste(unique(collectionVariableDetails$inferredVariable$entityId),collectionVariableDetails$collectionVariablePlotRef, sep='.')
      value.name <- paste(unique(collectionVariableDetails$inferredVariable$entityId),collectionVariableDetails$inferredVariable$variableId, sep='.')
    }

    # Reshape data
    .dt <- data.table::melt(.dt, measure.vars = veupathUtils::toColNameOrNull(collectionVariable),
                        variable.factor = FALSE,
                        variable.name= variable.name,
                        value.name=value.name)

    veupathUtils::logWithTime('Data reshaped according to collectionVariable.', verbose)

    # Replace collectionVar values (previously column names) with display labels or variableId
    .dt[[variable.name]] <- lapply(.dt[[variable.name]], toIdOrDisplayLabel, collectionVariable)

    # Assign new variable details for the created categorical variable
    newCatVariable <- list('variableId' = collectionVariableDetails$collectionVariablePlotRef,
                   'entityId' = unique(collectionVariable$entityId),
                   'dataType' = 'STRING',
                   'dataShape' = 'CATEGORICAL')

    if (collectionVariableDetails$collectionVariablePlotRef == 'xAxisVariable') {
      xAxisVariable <- newCatVariable
      x <- veupathUtils::toColNameOrNull(xAxisVariable)
      xType <- veupathUtils::toStringOrNull(as.character(xAxisVariable$dataType))
      xShape <- veupathUtils::toStringOrNull(as.character(xAxisVariable$dataShape))
      .dt[[x]] <- updateType(.dt[[x]], xType)

    } else if (collectionVariableDetails$collectionVariablePlotRef == 'overlayVariable') {
      overlayVariable <- newCatVariable
      group <- veupathUtils::toColNameOrNull(overlayVariable)
      groupType <- veupathUtils::toStringOrNull(as.character(overlayVariable$dataType))
      .dt[[group]] <- updateType(.dt[[group]], groupType)

    } else if (collectionVariableDetails$collectionVariablePlotRef == 'facetVariable1') {
      facetVariable1 <- newCatVariable
      facet1 <- veupathUtils::toColNameOrNull(facetVariable1)
      facetType1 <- veupathUtils::toStringOrNull(as.character(facetVariable1$dataType))
      .dt[[facet1]] <- updateType(.dt[[facet1]], facetType1) 

      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]

    } else if (collectionVariableDetails$collectionVariablePlotRef == 'facetVariable2') {
      facetVariable2 <- newCatVariable
      facet2 <- veupathUtils::toColNameOrNull(facetVariable2)
      facetType2 <- veupathUtils::toStringOrNull(as.character(facetVariable2$dataType))
      .dt[[facet2]] <- updateType(.dt[[facet2]], facetType2) 

      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]
      if (uniqueN(.dt[[panel]]) > 25) stop("Maximum number of panels allowed is 25.")
    }

    # Assume inferredVarPlotRef = yAxisVariable always.
    yAxisVariable <- collectionVariableDetails$inferredVariable
    y <- veupathUtils::toColNameOrNull(yAxisVariable)
    yType <- veupathUtils::toStringOrNull(as.character(yAxisVariable$dataType))
    .dt[[y]] <- updateType(.dt[[y]], yType) 

    data.table::setcolorder(.dt, c(x, y, z, group, panel))

    veupathUtils::logWithTime('Handling of collectionVariables complete.', verbose)

  }

  # Update types
  .dt[[x]] <- updateType(.dt[[x]], xType)
  if (!is.null(y)) { .dt[[y]] <- updateType(.dt[[y]], yType) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType) }
  if (!is.null(group)) { .dt[[group]] <- updateType(.dt[[group]], groupType) }
  if (!is.null(panel)) { .dt[[panel]] <- updateType(.dt[[panel]], 'STRING') }
  veupathUtils::logWithTime('Base data types updated for all columns as necessary.', verbose)

  completeCasesAllVars <- jsonlite::unbox(nrow(.dt[complete.cases(.dt),]))
  completeCasesAxesVars <- jsonlite::unbox(nrow(.dt[complete.cases(.dt[, c(x,y), with=FALSE]),]))
  veupathUtils::logWithTime('Determined total number of complete cases across axes and strata vars.', verbose)

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

  veupathUtils::logWithTime('Calculated sample sizes per group.', verbose)

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
  if (!is.null(collectionVariable)) { attr$collectionVariable <- collectionVariable }
  if (!all(unlist(lapply(computedVariableMetadata, is.null)))) { attr$computedVariableMetadata <- computedVariableMetadata}

  veupathUtils::setAttrFromList(.dt, attr)
  .pd <- validatePlotdata(.dt)
  veupathUtils::logWithTime('Base plot.data object created.', verbose)

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
  stopifnot(veupathUtils::toColNameOrNull(xAxisVariable) %in% names(.dt))
  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

# Additional accessor functions
sampleSizeTable <- function(.pd) { attr(.pd, 'sampleSizeTable') }
completeCasesTable <- function(.pd) { attr(.pd, 'completeCasesTable')}
#these helpers need either validation or to be a dedicated method
statsTable <- function(.pd) { attr(.pd, 'statsTable') }

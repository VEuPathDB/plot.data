makeVariableDetails <- function(value, variableId, entityId, displayLabel = NULL) {
  if (!is.null(value)) {
    if (length(value) == 1) {
      variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=jsonlite::unbox(as.character(value)))
    } else {
      variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=as.character(value))
    }
  } else {
    if (length(variableId) > 1) {
      variableDetails <- list('variableId'=variableId, 'entityId'=entityId)
    } else {
      variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId))
    }
  }

  if (!is.null(displayLabel)) {
    variableDetails$displayLabel <- jsonlite::unbox(displayLabel)
  }

  return(variableDetails)
}

#intended for table attrs that dont conform to the one row per group structure, but rather one row per var
addVariableDetailsToColumn <- function(.pd, variableIdColName) {
  namedAttrList <- getPDAttributes(.pd)
   
  # Add variable details for any variable in the variableIdCol
  if ('xAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$xAxisVariable)] <- list(makeVariableDetails(NULL, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId, namedAttrList$xAxisVariable$displayLabel))
  if ('yAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$yAxisVariable)] <- list(makeVariableDetails(NULL, namedAttrList$yAxisVariable$variableId, namedAttrList$yAxisVariable$entityId, namedAttrList$yAxisVariable$displayLabel))
  if ('zAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$zAxisVariable)] <- list(makeVariableDetails(NULL, namedAttrList$zAxisVariable$variableId, namedAttrList$zAxisVariable$entityId, namedAttrList$zAxisVariable$displayLabel))
  if ('overlayVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$overlayVariable)] <- list(makeVariableDetails(NULL, namedAttrList$overlayVariable$variableId, namedAttrList$overlayVariable$entityId, namedAttrList$overlayVariable$displayLabel))
  if ('facetVariable1' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$facetVariable1)] <- list(makeVariableDetails(NULL, namedAttrList$facetVariable1$variableId, namedAttrList$facetVariable1$entityId, namedAttrList$facetVariable1$displayLabel))
  if ('facetVariable2' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == veupathUtils::toColNameOrNull(namedAttrList$facetVariable2)] <- list(makeVariableDetails(NULL, namedAttrList$facetVariable2$variableId, namedAttrList$facetVariable2$entityId, namedAttrList$facetVariable2$displayLabel))
  if ('collectionVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] %in% veupathUtils::toColNameOrNull(namedAttrList$collectionVariable)] <- lapply(seq_along(namedAttrList$collectionVariable$variableId), function(varInd) {makeVariableDetails(NULL, namedAttrList$collectionVariable$variableId[varInd], namedAttrList$collectionVariable$entityId[varInd], namedAttrList$collectionVariable$displayLabel[varInd])})
  
  return(.pd)
}

addStrataVariableDetails <- function(.pd) {
  namedAttrList <- getPDAttributes(.pd)
  group <- veupathUtils::toColNameOrNull(namedAttrList$overlayVariable)
  facet1 <- veupathUtils::toColNameOrNull(namedAttrList$facetVariable1)
  facet2 <- veupathUtils::toColNameOrNull(namedAttrList$facetVariable2)
 
  # !!!!! work off a copy while writing json
  # since we have two exported fxns, dont want calling one changing the result of the other
  if (!is.null(group)) {
    if (!identical(namedAttrList$overlayVariable$dataShape, 'CONTINUOUS') & (group %in% names(.pd))) {
      names(.pd)[names(.pd) == group] <- 'overlayVariableDetails'
      .pd$overlayVariableDetails <- lapply(.pd$overlayVariableDetails, makeVariableDetails, namedAttrList$overlayVariable$variableId, namedAttrList$overlayVariable$entityId, namedAttrList$overlayVariable$displayLabel)
      #if (nrow(.pd) == 1) { .pd$overlayVariableDetails <- list(list(.pd$overlayVariableDetails)) }
    }
  }

  if (!is.null(facet1) & !is.null(facet2)) {
    names(.pd)[names(.pd) == 'panel'] <- 'facetVariableDetails'
    .pd$facetVariableDetails <- Map(list, lapply(veupathUtils::strSplit(.pd$facetVariableDetails, '.||.'), makeVariableDetails, namedAttrList$facetVariable1$variableId, namedAttrList$facetVariable1$entityId, namedAttrList$facetVariable1$displayLabel), lapply(veupathUtils::strSplit(.pd$facetVariableDetails, '.||.', index=2), makeVariableDetails, namedAttrList$facetVariable2$variableId, namedAttrList$facetVariable2$entityId, namedAttrList$facetVariable2$displayLabel))
  } else {
    if (!is.null(facet1)) {
      names(.pd)[names(.pd) == facet1] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, namedAttrList$facetVariable1$variableId, namedAttrList$facetVariable1$entityId, namedAttrList$facetVariable1$displayLabel), list)
    } else if (!is.null(facet2)) {
      names(.pd)[names(.pd) == facet2] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, namedAttrList$facetVariable2$variableId, namedAttrList$facetVariable2$entityId, namedAttrList$facetVariable2$displayLabel), list)
    } 
  }
  if (nrow(.pd) == 1 && 'facetVariableDetails' %in% names(.pd)) { 
    .pd$facetVariableDetails <- list(list(.pd$facetVariableDetails)) 
  }

  return(.pd)
}

getJSON <- function(.pd, evilMode) {
  namedAttrList <- getPDAttributes(.pd)
  class <- attr(.pd, 'class')[1] 

  if ('statsTable' %in% names(namedAttrList)) {
    statsTable <- statsTable(.pd)
    namedAttrList$statsTable <- NULL
    attr <- attributes(statsTable)
    statsTable <- veupathUtils::setAttrFromList(statsTable, namedAttrList, removeExtraAttrs=F)
    statsTable <- addStrataVariableDetails(statsTable)
    attr$names <- names(statsTable)
    statsTable <- veupathUtils::setAttrFromList(statsTable, attr)
    if (veupathUtils::toColNameOrNull(namedAttrList$xAxisVariable) %in% names(statsTable)) {
      x <- veupathUtils::toColNameOrNull(namedAttrList$xAxisVariable)
      names(statsTable)[names(statsTable) == x] <- 'xVariableDetails'
      statsTable$xVariableDetails <- lapply(statsTable$xVariableDetails, makeVariableDetails, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId, namedAttrList$xAxisVariable$displayLabel)
    }
  }

  if ('sampleSizeTable' %in% names(namedAttrList)) {
    sampleSizeTable <- sampleSizeTable(.pd)
    namedAttrList$sampleSizeTable <- NULL
    attr <- attributes(sampleSizeTable)
    sampleSizeTable <- veupathUtils::setAttrFromList(sampleSizeTable, namedAttrList, removeExtraAttrs=F)
    sampleSizeTable <- addStrataVariableDetails(sampleSizeTable)
    attr$names <- names(sampleSizeTable)
    sampleSizeTable <- veupathUtils::setAttrFromList(sampleSizeTable, attr)
    if ('xAxisVariable' %in% names(namedAttrList)) {
      if (namedAttrList$xAxisVariable$dataShape != "CONTINUOUS") {
        x <- veupathUtils::toColNameOrNull(namedAttrList$xAxisVariable)
        names(sampleSizeTable)[names(sampleSizeTable) == x] <- 'xVariableDetails'
        sampleSizeTable$xVariableDetails <- lapply(sampleSizeTable$xVariableDetails, makeVariableDetails, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId, namedAttrList$xAxisVariable$displayLabel)
      }
    }
  }
  
  if ('completeCasesTable' %in% names(namedAttrList)) {
    completeCasesTable <- completeCasesTable(.pd)
    namedAttrList$completeCasesTable <- NULL
    attr <- attributes(completeCasesTable)
    completeCasesTable <- veupathUtils::setAttrFromList(completeCasesTable, namedAttrList, removeExtraAttrs = F)
    completeCasesTable <- addVariableDetailsToColumn(completeCasesTable, 'variableDetails')
    attr$names <- names(completeCasesTable)
    completeCasesTable <- veupathUtils::setAttrFromList(completeCasesTable, attr)
  }

  if ('xAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$xVariableDetails <- makeVariableDetails(NULL, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId,  namedAttrList$xAxisVariable$displayLabel)
    namedAttrList$xAxisVariable <- NULL
  }

  if ('yAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$yVariableDetails <- makeVariableDetails(NULL, namedAttrList$yAxisVariable$variableId, namedAttrList$yAxisVariable$entityId,  namedAttrList$yAxisVariable$displayLabel)
    namedAttrList$yAxisVariable <- NULL
  }
  if ('zAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$zVariableDetails <- makeVariableDetails(NULL, namedAttrList$zAxisVariable$variableId, namedAttrList$zAxisVariable$entityId, namedAttrList$zAxisVariable$displayLabel)
    namedAttrList$zAxisVariable <- NULL
  }
  # if ('collectionVariable' %in% names(namedAttrList)) {
  #   namedAttrList$collectionVariableDetails <- makeVariableDetails(NULL, namedAttrList$collectionVariable$variableId, namedAttrList$collectionVariable$entityId, namedAttrList$collectionVariable$displayLabel)
  #   namedAttrList$collectionVariable <- NULL
  # }
  
  .pd <- addStrataVariableDetails(.pd)
  # If overlay is continuous, handle similarly to x, y, z vars.
  if ('overlayVariable' %in% names(namedAttrList) & identical(namedAttrList$overlayVariable$dataShape, 'CONTINUOUS')) {
    namedAttrList$overlayVariableDetails <- makeVariableDetails(NULL, namedAttrList$overlayVariable$variableId, namedAttrList$overlayVariable$entityId, namedAttrList$overlayVariable$displayLabel)
  }
  
  namedAttrList$facetVariable1 <- NULL
  namedAttrList$facetVariable2 <- NULL
  namedAttrList$overlayVariable <- NULL

  # Ensure computedVariableMetadata meets api
  if ('computedVariableMetadata' %in% names(namedAttrList)) {

    computedVariableMetadata <- namedAttrList$computedVariableMetadata

    # Note - returning range min and max as strings in order to better handle dates.
    if (!is.null(computedVariableMetadata$displayName)) {computedVariableMetadata$displayName <- as.character(computedVariableMetadata$displayName)}
    if (!is.null(computedVariableMetadata$displayRangeMin)) {computedVariableMetadata$displayRangeMin <- jsonlite::unbox(as.character(computedVariableMetadata$displayRangeMin))}
    if (!is.null(computedVariableMetadata$displayRangeMax)) {computedVariableMetadata$displayRangeMax <- jsonlite::unbox(as.character(computedVariableMetadata$displayRangeMax))}
    if (!is.null(computedVariableMetadata$collectionVariable$collectionType)) {computedVariableMetadata$collectionVariable$collectionType <- jsonlite::unbox(as.character(computedVariableMetadata$collectionVariable$collectionType))}
    
    # Include collection variable details in compute metadata for now
    if ('collectionVariable' %in% names(namedAttrList)) {
      computedVariableMetadata$collectionVariable$collectionVariablePlotRef <- jsonlite::unbox(namedAttrList$collectionVariable$collectionVariablePlotRef)
      computedVariableMetadata$collectionVariable$collectionValuePlotRef <- jsonlite::unbox(namedAttrList$collectionVariable$collectionValuePlotRef)
      
      computedVariableMetadata$collectionVariable$collectionVariableDetails <- lapply(seq_along(namedAttrList$collectionVariable$variableId), function(varInd) {makeVariableDetails(NULL, namedAttrList$collectionVariable$variableId[varInd], namedAttrList$collectionVariable$entityId[varInd], namedAttrList$collectionVariable$displayLabel[varInd])})

      namedAttrList$collectionVariable <- NULL
    }

    namedAttrList$computedVariableMetadata <- computedVariableMetadata
  }
  
  outList <- list(class = list('data'=.pd, 'config'=namedAttrList))
  if (!inherits(sampleSizeTable, 'function')) {
    outList$sampleSizeTable <- sampleSizeTable
  }
  if (!inherits(statsTable, 'function')) {
    outList$statsTable <- statsTable
  }
  if (!inherits(completeCasesTable, 'function')) {
    outList$completeCasesTable <- completeCasesTable
  }

  names(outList)[1] <- class
  outJson <- jsonlite::toJSON(outList, na='null')

  return(outJson)
}

#' Write json to local tmp file
#'
#' This function returns the name of a json file which it has
#' written a data.table object out to.
#' @param .pd a data.table to convert to json and write to a tmp file
#' @param pattern optional tmp file prefix
#' @return character name of a tmp file w ext *.json
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite prettify
#' @export
writeJSON <- function(.pd, evilMode, pattern = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  outJson <- getJSON(.pd, evilMode)
  if (is.null(pattern)) { 
    pattern <- attr(.pd, 'class')[1]
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)
  veupathUtils::logWithTime(paste('New output file written:', outFileName), verbose)

  return(outFileName)
}


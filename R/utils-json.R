makeVariableDetails <- function(value, variableId, entityId) {
  if (!is.null(value)) {
    if (length(value) == 1) {
      variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=jsonlite::unbox(value))
    } else {
      variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=value)
    }
  } else {
    variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId))
  }

  return(variableDetails)
}

addVariableDetails <- function(.pd, variableId, varDetailsName, entityId) {
  data.table::setnames(.pd, variableId, varDetailsName)
  .pd[[varDetailsName]] <- lapply(.pd[[varDetailsName]], makeVariableDetails, variableId, entityId)

  return(.pd)
}

addStrataVariableDetails <- function(.pd) {
  namedAttrList <- getPDAttributes(.pd)
  group <- NULL
  facet1 <- NULL
  facet2 <- NULL
  if ('overlayVariable' %in% names(namedAttrList)) { group <- namedAttrList$overlayVariable$variableId }
  if ('facetVariable1' %in% names(namedAttrList)) { facet1 <- namedAttrList$facetVariable1$variableId }
  if ('facetVariable2' %in% names(namedAttrList)) { facet2 <- namedAttrList$facetVariable2$variableId }
  
  if (!is.null(group)) {
    data.table::setnames(.pd, group, 'overlayVariableDetails')
    .pd$overlayVariableDetails <- lapply(.pd$overlayVariableDetails, makeVariableDetails, group, namedAttrList$overlayVariable$entityId)
  }

  if (!is.null(facet1) & !is.null(facet2)) {
    data.table::setnames(.pd, 'panel', 'facetVariableDetails')
    .pd$facetVariableDetails <- Map(list, lapply(strSplit(.pd$facetVariableDetails, '.||.'), makeVariableDetails, facet1, namedAttrList$facetVarialbe1$entityId), lapply(strSplit(.pd$facetVariableDetails, '.||.', index=2), makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId))
  } else {
    if (!is.null(facet1)) {
      data.table::setnames(.pd, facet1, 'facetVariableDetails')
      .pd$facetVariableDetails <- lapply(.pd$facetVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId)
    } else if (!is.null(facet2)) {
      data.table::setnames(.pd, facet2, 'facetVariableDetails')
      .pd$facetVariableDetails <- lapply(.pd$facetVariableDetails, makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId)
    }
  }

  return(.pd)
}

getJSON <- function(.pd) {
  namedAttrList <- getPDAttributes(.pd)
  class <- attr(.pd, 'class')[1] 

  if ('statsTable' %in% names(namedAttrList)) {
    statsTable <- statsTable(.pd)
    namedAttrList$statsTable <- NULL
    attr <- attributes(statsTable)
    statsTable <- setAttrFromList(statsTable, namedAttrList, removeExtraAttrs=F)
    statsTable <- addStrataVariableDetails(statsTable)
    attr$names <- names(statsTable)
    statsTable <- setAttrFromList(statsTable, attr)
  }

  if ('sampleSizeTable' %in% names(namedAttrList)) {
    sampleSizeTable <- sampleSizeTable(.pd)
    namedAttrList$sampleSizeTable <- NULL
    attr <- attributes(sampleSizeTable)
    sampleSizeTable <- setAttrFromList(sampleSizeTable, namedAttrList, removeExtraAttrs=F)
    sampleSizeTable <- addStrataVariableDetails(sampleSizeTable)
    attr$names <- names(sampleSizeTable)
    sampleSizeTable <- setAttrFromList(sampleSizeTable, attr)
    if ('xAxisVariable' %in% names(namedAttrList)) {
      if (namedAttrList$xAxisVariable$dataType == 'STRING') {
        sampleSizeTable <- addVariableDetails(sampleSizeTable, namedAttrList$xAxisVariable$variableId, 'xVariableDetails', namedAttrList$xAxisVariable$entityId)
      }
    }
  }
  
  if ('completeCasesTable' %in% names(namedAttrList)) {
    completeCasesTable <- completeCasesTable(.pd)
    namedAttrList$completeCasesTable <- NULL
    attr <- attributes(completeCasesTable)
    completeCasesTable <- setAttrFromList(completeCasesTable, namedAttrList, removeExtraAttrs = F)
    
    # Add variable details for any variable in the completeCasesTable
    if ('xAxisVariable' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$xAxisVariable$variableId, 'xVariableDetails', namedAttrList$xAxisVariable$entityId)
    if ('yAxisVariable' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$yAxisVariable$variableId, 'yVariableDetails', namedAttrList$yAxisVariable$entityId)
    if ('zAxisVariable' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$zAxisVariable$variableId, 'zVariableDetails', namedAttrList$zAxisVariable$entityId)
    if ('overlayVariable' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$overlayVariable$variableId, 'overlayVariableDetails', namedAttrList$overlayVariable$entityId)
    if ('facetVariable1' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$facetVariable1$variableId, 'facetVariable1Details', namedAttrList$facetVariable1$entityId)
    if ('facetVariable2' %in% names(namedAttrList)) completeCasesTable <- addVariableDetails(completeCasesTable, namedAttrList$facetVariable2$variableId, 'facetVariable2Details', namedAttrList$facetVariable2$entityId)
    
    attr$names <- names(completeCasesTable)
    completeCasesTable <- setAttrFromList(completeCasesTable, attr)
  }

  .pd <- addStrataVariableDetails(.pd)
  namedAttrList$overlayVariable <- NULL
  namedAttrList$facetVariable1 <- NULL
  namedAttrList$facetVariable2 <- NULL

  if ('xAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$xVariableDetails <- makeVariableDetails(NULL, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId)
    namedAttrList$xAxisVariable <- NULL
  }
  if ('yAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$yVariableDetails <- makeVariableDetails(NULL, namedAttrList$yAxisVariable$variableId, namedAttrList$yAxisVariable$entityId)
    namedAttrList$yAxisVariable <- NULL
  }
  if ('zAxisVariable' %in% names(namedAttrList)) {
    namedAttrList$zVariableDetails <- makeVariableDetails(NULL, namedAttrList$zAxisVariable$variableId, namedAttrList$zAxisVariable$entityId)
    namedAttrList$zAxisVariable <- NULL
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
  outJson <- jsonlite::toJSON(outList)

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
writeJSON <- function(.pd, pattern = NULL) {
  outJson <- getJSON(.pd)
  if (is.null(pattern)) { 
    pattern <- attr(.pd, 'class')[1]
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)

  return(outFileName)
}


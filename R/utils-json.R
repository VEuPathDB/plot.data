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

#intended for table attrs that dont conform to the one row per group structure, but rather one row per var
addVariableDetailsToColumn <- function(.pd, variableIdColName) {
  namedAttrList <- getPDAttributes(.pd)
   
  # Add variable details for any variable in the variableIdCol
  if ('xAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$xAxisVariable$variableId] <- list(makeVariableDetails(NULL, namedAttrList$xAxisVariable$variableId, namedAttrList$xAxisVariable$entityId))
  if ('yAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$yAxisVariable$variableId] <- list(makeVariableDetails(NULL, namedAttrList$yAxisVariable$variableId, namedAttrList$yAxisVariable$entityId))
  if ('zAxisVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$zAxisVariable$variableId] <- list(makeVariableDetails(NULL, namedAttrList$zAxisVariable$variableId, namedAttrList$zAxisVariable$entityId))
  if ('overlayVariable' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$overlayVariable$variableId] <- list(makeVariableDetails(NULL, namedAttrList$overlayVariable$variableId, namedAttrList$overlayVariable$entityId))
  if ('facetVariable1' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$facetVariable1$variableId] <- list(makeVariableDetails(NULL, namedAttrList$facetVariable1$variableId, namedAttrList$facetVariable1$entityId))
  if ('facetVariable2' %in% names(namedAttrList)) .pd[[variableIdColName]][.pd[[variableIdColName]] == namedAttrList$facetVariable2$variableId] <- list(makeVariableDetails(NULL, namedAttrList$facetVariable2$variableId, namedAttrList$facetVariable2$entityId))

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
 
  # !!!!! work off a copy while writing json
  # since we have two exported fxns, dont want calling one changing the result of the other
  if (!is.null(group)) {
    if (!identical(namedAttrList$overlayVariable$dataShape, 'CONTINUOUS')) {
      names(.pd)[names(.pd) == group] <- 'overlayVariableDetails'
      .pd$overlayVariableDetails <- lapply(.pd$overlayVariableDetails, makeVariableDetails, group, namedAttrList$overlayVariable$entityId)
    }
  }

  if (!is.null(facet1) & !is.null(facet2)) {
    names(.pd)[names(.pd) == 'panel'] <- 'facetVariableDetails'
    .pd$facetVariableDetails <- Map(list, lapply(strSplit(.pd$facetVariableDetails, '.||.'), makeVariableDetails, facet1, namedAttrList$facetVarialbe1$entityId), lapply(strSplit(.pd$facetVariableDetails, '.||.', index=2), makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId))
  } else {
    if (!is.null(facet1)) {
      names(.pd)[names(.pd) == facet1] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId), list)
    } else if (!is.null(facet2)) {
      names(.pd)[names(.pd) == facet2] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId), list)
    }
  }

  return(.pd)
}

getJSON <- function(.pd, evilMode) {
  namedAttrList <- getPDAttributes(.pd)
  class <- attr(.pd, 'class')[1] 

  if (!evilMode && 'statsTable' %in% names(namedAttrList)) {
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
        x <- namedAttrList$xAxisVariable$variableId
        names(sampleSizeTable)[names(sampleSizeTable) == x] <- 'xVariableDetails'
        sampleSizeTable$xVariableDetails <- lapply(sampleSizeTable$xVariableDetails, makeVariableDetails, x, namedAttrList$xAxisVariable$entityId)

      }
    }
  }
  
  if ('completeCasesTable' %in% names(namedAttrList)) {
    completeCasesTable <- completeCasesTable(.pd)
    namedAttrList$completeCasesTable <- NULL
    attr <- attributes(completeCasesTable)
    completeCasesTable <- setAttrFromList(completeCasesTable, namedAttrList, removeExtraAttrs = F)
    completeCasesTable <- addVariableDetailsToColumn(completeCasesTable, 'variableDetails')
    attr$names <- names(completeCasesTable)
    completeCasesTable <- setAttrFromList(completeCasesTable, attr)
  }

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
  
  .pd <- addStrataVariableDetails(.pd)
  # If overlay is continuous, handle similarly to x, y, z vars.
  if ('overlayVariable' %in% names(namedAttrList) & identical(namedAttrList$overlayVariable$dataShape, 'CONTINUOUS')) {
    namedAttrList$overlayVariableDetails <- makeVariableDetails(NULL, namedAttrList$overlayVariable$variableId, namedAttrList$overlayVariable$entityId)
  }
  
  namedAttrList$facetVariable1 <- NULL
  namedAttrList$facetVariable2 <- NULL
  namedAttrList$overlayVariable <- NULL

  

  
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
writeJSON <- function(.pd, evilMode, pattern = NULL) {
  outJson <- getJSON(.pd, evilMode)
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


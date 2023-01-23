setGeneric("makeVariableDetails", 
  function(value, object) standardGeneric("makeVariableDetails"),
  signature = "object"
)

#' @export
setMethod("makeVariableDetails", signature("VariableSpec"), function(value, object) {
  variableId <- object@variableId
  entityId <- object@entityId

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

  return(variableDetails)    
})

setMethod("makeVariableDetails", signature("VariableMetadata"), function(value, object) {
  variableId <- object@variableSpec@variableId
  entityId <- object@variableSpec@entityId
  displayLabel <- object@displayName
  if (is.na(displayLabel)) displayLabel <- NULL

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
})

#intended for table attrs that dont conform to the one row per group structure, but rather one row per var
addVariableDetailsToColumn <- function(.pd, variableIdColName) {
  namedAttrList <- getPDAttributes(.pd)
  plotRefs <- unlist(lapply(as.list(namedAttrList$variables), function(x) {x@plotReference@value}))
   
  # Add variable details for any variable in the variableIdCol
  if ('xAxis' %in% plotRefs) {
    x <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'xAxis')
    if (x %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == x] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'xAxis')))
      plotRefs <- plotRefs[plotRefs != 'xAxis']
    }
  }

  if ('yAxis' %in% plotRefs) {
    y <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'yAxis')
    if (y %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == y] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'yAxis')))
      plotRefs <- plotRefs[plotRefs != 'yAxis']
    }
  }

  if ('zAxis' %in% plotRefs) {
    z <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'zAxis')
    if (z %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == z] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'zAxis')))
      plotRefs <- plotRefs[plotRefs != 'zAxis']
    }
  }

  if ('overlay' %in% plotRefs) {
    group <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'overlay')
    if (group %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == group] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'overlay')))
      plotRefs <- plotRefs[plotRefs != 'overlay']
    }
  }

  if ('facet1' %in% plotRefs) {
    facet1 <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'facet1')
    if (facet1 %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == facet1] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet1')))
      plotRefs <- plotRefs[plotRefs != 'facet1']
    }
  }

  if ('facet2' %in% plotRefs) {
    facet2 <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'facet2')
    if (facet2 %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == facet2] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet2')))
      plotRefs <- plotRefs[plotRefs != 'facet2']
    }
  }

  if ('geo' %in% plotRefs) {
    geo <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'geo')
    if (geo %in% .pd[[variableIdColName]]) {
      .pd[[variableIdColName]][.pd[[variableIdColName]] == geo] <- list(makeVariableDetails(NULL, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'geo')))
      plotRefs <- plotRefs[plotRefs != 'geo']
    }
  }

  if (!!length(plotRefs)) {
    collectionPlotRef <- plotRefs[!plotRefs %in% c('yAxis','latitude','longitude')]
    if (!!length(collectionPlotRef)) {
      collectionVM <- veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, collectionPlotRef)
      collectionColNames <- paste0(collectionVM@variableSpec@entityId, '.', collectionVM@vocabulary)
      collectionVarSpecs <- lapply(collectionVM@vocabulary, function(x) { veupathUtils::VariableSpec(variableId = x, entityId = collectionVM@variableSpec@entityId) })
      .pd[[variableIdColName]][.pd[[variableIdColName]] %in% collectionColNames] <- lapply(as.list(collectionVarSpecs), function(x) {makeVariableDetails(NULL, x)})
    }
  }

  return(.pd)
}

addStrataVariableDetails <- function(.pd, useGradientColorscale=FALSE) {
  namedAttrList <- getPDAttributes(.pd)
  group <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'overlay')
  facet1 <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'facet1')
  facet2 <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'facet2')
  geo <- veupathUtils::findColNamesFromPlotRef(namedAttrList$variables, 'geo')

  # !!!!! work off a copy while writing json
  # since we have two exported fxns, dont want calling one changing the result of the other
  if (!is.null(group) && !useGradientColorscale && (group %in% names(.pd))) {
    names(.pd)[names(.pd) == group] <- 'overlayVariableDetails'
    .pd$overlayVariableDetails <- lapply(.pd$overlayVariableDetails, makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'overlay'))
  }

  if (!is.null(geo)) {
    names(.pd)[names(.pd) == geo] <- 'geoAggregateVariableDetails'
    .pd$geoAggregateVariableDetails <- lapply(.pd$geoAggregateVariableDetails, makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'geo'))
  }

  if (!is.null(facet1) & !is.null(facet2)) {
    names(.pd)[names(.pd) == 'panel'] <- 'facetVariableDetails' 
    .pd$facetVariableDetails <- Map(list, lapply(veupathUtils::strSplit(.pd$facetVariableDetails, '.||.'), makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet1')), lapply(veupathUtils::strSplit(.pd$facetVariableDetails, '.||.', index=2), makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet2')))
  } else {
    if (!is.null(facet1)) {
      names(.pd)[names(.pd) == facet1] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet1')), list)
    } else if (!is.null(facet2)) {
      names(.pd)[names(.pd) == facet2] <- 'facetVariableDetails'
      .pd$facetVariableDetails <- lapply(lapply(.pd$facetVariableDetails, makeVariableDetails, veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'facet2')), list)
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
  useGradientColorscale <- ifelse(identical(namedAttrList$useGradientColorscale, TRUE), TRUE, FALSE)

  if ('statsTable' %in% names(namedAttrList)) {
    statsTable <- statsTable(.pd)
    namedAttrList$statsTable <- NULL
    attr <- attributes(statsTable)
    statsTable <- veupathUtils::setAttrFromList(statsTable, namedAttrList, removeExtraAttrs=F)
    statsTable <- addStrataVariableDetails(statsTable, useGradientColorscale)
    attr$names <- names(statsTable)
    statsTable <- veupathUtils::setAttrFromList(statsTable, attr)
    xVM <- veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'xAxis')
    if (!is.null(xVM)) {
      x <- veupathUtils::getColName(xVM@variableSpec)
      if (x %in% names(statsTable)) {
        names(statsTable)[names(statsTable) == x] <- 'xVariableDetails'
        statsTable$xVariableDetails <- lapply(statsTable$xVariableDetails, makeVariableDetails, xVM)
      }
    }
  }

  # the new 2x2 stats table has a different api, so this workaround lets us maintain two different apis until they unite
  if ('allStatsTable' %in% names(namedAttrList)) {
    statsTable <- attributes(.pd)$allStatsTable
    namedAttrList$allStatsTable <- NULL
    
  }

  if ('sampleSizeTable' %in% names(namedAttrList)) {
    sampleSizeTable <- sampleSizeTable(.pd)
    namedAttrList$sampleSizeTable <- NULL
    attr <- attributes(sampleSizeTable)
    sampleSizeTable <- veupathUtils::setAttrFromList(sampleSizeTable, namedAttrList, removeExtraAttrs=F)
    sampleSizeTable <- addStrataVariableDetails(sampleSizeTable, useGradientColorscale)
    attr$names <- names(sampleSizeTable)
    sampleSizeTable <- veupathUtils::setAttrFromList(sampleSizeTable, attr)
    xVM <- veupathUtils::findVariableMetadataFromPlotRef(namedAttrList$variables, 'xAxis')
    if (!is.null(xVM)) {
      x <- veupathUtils::getColName(xVM@variableSpec)
      if (x %in% names(sampleSizeTable)) {
        names(sampleSizeTable)[names(sampleSizeTable) == x] <- 'xVariableDetails'
        sampleSizeTable$xVariableDetails <- lapply(sampleSizeTable$xVariableDetails, makeVariableDetails, xVM)
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

  .pd <- addStrataVariableDetails(.pd, useGradientColorscale)

  # Remove useGradientColorscale and handle variables attr separately
  namedAttrList$useGradientColorscale <- NULL
  # id love if this worked but its not quite right
  #variablesJSON <- veupathUtils::toJSON(namedAttrList$variables, named = FALSE)
  #namedAttrList$variables <- jsonlite::fromJSON(variablesJSON)
  variables <- namedAttrList$variables
  namedAttrList$variables <- NULL

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

  outJson <- gsub('"config":{', paste0('"config":{"variables":', veupathUtils::toJSON(variables, named = FALSE), ","), outJson, fixed = TRUE)

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


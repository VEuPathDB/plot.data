makeVariableDetails <- function(value, variableId, entityId) {
  if (!is.null(value)) {
    variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId), 'value'=jsonlite::unbox(value))
  } else {
    variableDetails <- list('variableId'=jsonlite::unbox(variableId), 'entityId'=jsonlite::unbox(entityId))
  }

  return(variableDetails)
}

addStrataVariableDetails <- function(.pd) {
  namedAttrList <- getPDAttributes(.pd)
  group <- NULL
  facet1 <- NULL
  facet2 <- NULL
  if ('overlayVariable' %in% names(namedAttrList)) { group <- namedAttrList$overlayVariable$variableId }
  if ('facetVariable1' %in% names(namedAttrList)) { facet1 <- namedAttrList$facetVariable1$variableId }
  if ('facetVariable2' %in% names(namedAttrList)) { facet2 <- namedAttrList$facetVariable2$variableId }

  if (!is.null(facet1) & !is.null(facet2)) {
    names(.pd)[names(.pd) == 'panel'] <- 'facetVariableDetails'
    .pd$facetVariableDetails <- lapply(.pd$facetVariableDetails, makeVariableDetails, list(facet1,facet2), map$entityId[map$id %in% c(facet1, facet2)])
  } else if (!is.null(group)) {
    names(.pd)[names(.pd) == group] <- 'overlayVariableDetails'
    .pd$overlayVariableDetails <- lapply(.pd$overlayVariableDetails, makeVariableDetails, group, namedAttrList$overlayVariable$entityId)
  } else if (!is.null(facet1)) {
    names(.pd)[names(.pd) == facet1] <- 'facetVariableDetails'
    .pd$facetVariableDetails <- lapply(.pd$facetVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId)
  } else if (!is.null(facet2)) {
    names(.pd)[names(.pd) == facet2] <- 'facetVariableDetails'
    .pd$facetVariableDetails <- lapply(.pd$facetVariableDetails, makeVariableDetails, facet2, namedAttrList$facetVariable2$entityId)
  }

  return(.pd)
}

getJSON <- function(.pd) {
  namedAttrList <- getPDAttributes(.pd)

  if (any(c('overlayVariable', 'facetVariable1', 'facetVariable2') %in% names(namedAttrList))) {
    .pd <- addStrataVariableDetails(.pd)
    namedAttrList$overlayVariable <- NULL
    namedAttrList$facetVariable1 <- NULL
    namedAttrList$facetVariable2 <- NULL
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

  outJson <- jsonlite::toJSON(list('data'=.pd, 'config'=namedAttrList))

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
  # just for now for debugging
  #outJson <- jsonlite::prettify(outJson)
  if (is.null(pattern)) { pattern <- 'file' }
  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)

  return(outFileName)
}


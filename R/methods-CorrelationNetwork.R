# Methods for the CorrelationNetwork class

setGeneric("getCorrelationCoefThreshold", function(object) standardGeneric("getCorrelationCoefThreshold"))
setMethod("getCorrelationCoefThreshold", "CorrelationNetwork", function(object) object@correlationCoefThreshold)
setGeneric("getPValueThreshold", function(object) standardGeneric("getPValueThreshold"))
setMethod("getPValueThreshold", "CorrelationNetwork", function(object) object@pValueThreshold)

#' Prune Links by Correlation Coefficient
#' 
#' Removes links that have an absolute correlation coefficient below a
#' threshold. This is a convenience function that calls pruneCorrelationLinks.
#' @param net A CorrelationNetwork object
#' @param correlationCoefThreshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksByCorrelationCoef <- function(net, correlationCoefThreshold = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneCorrelationLinks(net = net, correlationCoefThreshold = correlationCoefThreshold, verbose = verbose))
}

#' Prune Links by P-Value
#' 
#' Removes links that have a p-value above a threshold. This is a convenience
#' function that calls pruneCorrelationLinks.
#' @param net A Network object
#' @param pValueThreshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksByPValue <- function(net, pValueThreshold = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneCorrelationLinks(net = net, pValueThreshold = pValueThreshold, verbose = verbose))
}

#' @rdname pruneCorrelationLinks
#' @aliases pruneCorrelationLinks,CorrelationNetwork-method
setMethod("pruneCorrelationLinks", "CorrelationNetwork", 
function(
  object, 
  correlationCoefThreshold = NULL, 
  pValueThreshold = NULL, 
  verbose = c(TRUE, FALSE)
) {
  verbose <- veupathUtils::matchArg(verbose)

  object@links <- pruneCorrelationLinks(
    object@links, 
    correlationCoefThreshold = correlationCoefThreshold, 
    pValueThreshold = pValueThreshold, 
    verbose = verbose
  )

  object@correlationCoefThreshold <- ifelse(is.null(correlationCoefThreshold), NA_real_, correlationCoefThreshold) 
  object@pValueThreshold <- ifelse(is.null(pValueThreshold), NA_real_, pValueThreshold) 

  if (length(object@links) == 0) return(new("CorrelationNetwork"))

  validObject(object)
  return(object)
})

toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert CorrelationNetwork object to JSON
#'
#' Converts a CorrelationNetwork object to JSON
#' @param object A CorrelationNetwork object
#' @param named boolean that declares if names should be included
setMethod(toJSONGeneric, "CorrelationNetwork", function(object, named = c(TRUE, FALSE)) {
  
  named <- veupathUtils::matchArg(named)    
  tmp <- character()

  nodes_json <- veupathUtils::toJSON(object@nodes, named = FALSE)
  links_json <- veupathUtils::toJSON(object@links, named = FALSE)

  tmp <- paste0('"nodes":', nodes_json, ',"links":', links_json)
  tmp <- paste0('"data":{', tmp, '}')
  tmp <- paste0(
    '{', tmp, 
    ',"config":{',
      '"variables":{', veupathUtils::toJSON(object@variableMapping, named = FALSE), '}',
      ',"correlationCoefThreshold":', jsonlite::toJSON(jsonlite::unbox(object@correlationCoefThreshold)),
      ',"pValueThreshold":', jsonlite::toJSON(jsonlite::unbox(object@pValueThreshold)),
    '}}')
  
  if (named) tmp <- paste0('{"network":', tmp, '}')

  return(tmp)
})

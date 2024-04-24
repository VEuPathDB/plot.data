# Methods for the CorrelationNetwork class

setGeneric("getCorrelationCoefThreshold", function(object) standardGeneric("getCorrelationCoefThreshold"))
setMethod("getCorrelationCoefThreshold", "CorrelationNetwork", function(object) object@correlationCoefThreshold)
setGeneric("getPValueThreshold", function(object) standardGeneric("getPValueThreshold"))
setMethod("getPValueThreshold", "CorrelationNetwork", function(object) object@pValueThreshold)


#' @include methods-CorrelationLinks.R
linkAboveCorrelationCoefThreshold <- function(link, threshold) {
  return(abs(correlationCoef(link)) >= threshold)
}

#' Prune Links by Correlation Coefficient
#' 
#' Removes links that have an absolute correlation coefficient below a
#' threshold. This is a convenience function that calls pruneLinksByPredicate.
#' @param net A CorrelationNetwork object
#' @param correlationCoefThreshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksByCorrelationCoef <- function(net, correlationCoefThreshold, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneLinksByPredicate(net = net, predicate = linkAboveCorrelationCoefThreshold, threshold = correlationCoefThreshold, verbose = verbose))
}


linkBelowPValueThreshold <- function(link, threshold) {
  return(pValue(link) <= threshold)
}

#' Prune Links by P-Value
#' 
#' Removes links that have a p-value above a threshold. This is a convenience
#' function that calls pruneLinksByPredicate.
#' @param net A Network object
#' @param pValueThreshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksBelowWeight <- function(net, threshold, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneLinksByPredicate(net = net, predicate = linkBelowPValueThreshold, threshold = pValueThreshold, verbose = verbose))
}

toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert CorrelationNetwork object to JSON
#' 
#' Converts a CorrelationNetwork object to JSON
#' @param object A Network object
#' @param named boolean that declares if names should be included
#' @export
setMethod(toJSONGeneric, "CorrelationNetwork", function(object, named = c(TRUE, FALSE)) {
  
  named <- veupathUtils::matchArg(named)    
  tmp <- character()

  nodes_json <- veupathUtils::toJSON(object@nodes, named = FALSE)
  links_json <- veupathUtils::toJSON(object@links, named = FALSE)

  ## TODO add thresholds to response, consider refactoring to use parent class toJSON somehow?
  tmp <- paste0('"nodes":', nodes_json, ',"links":', links_json)
  tmp <- paste0('"data":{', tmp, '}')
  tmp <- paste0('{', tmp, ',"config":{"variables":{', veupathUtils::toJSON(object@variableMapping, named = FALSE), '}}}')
  
  if (named) tmp <- paste0('{"network":', tmp, '}')

  return(tmp)
})

#' @rdname pruneCorrelationLinks
#' @aliases pruneCorrelationLinks,CorrelationNetwork-method
setMethod("pruneCorrelationLinks", "CorrelationNetwork", function(object, correlationCoefThreshold, pValueThreshold, verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  object@links <- pruneCorrelationLinks(
    object@links, 
    correlationCoefThreshold = correlationCoefThreshold, 
    pValueThreshold = pValueThreshold, 
    verbose = verbose
  )

  object@correlationCoefThreshold <- correlationCoefThreshold
  object@pValueThreshold <- pValueThreshold

  validObject(object)
  return(object)
})
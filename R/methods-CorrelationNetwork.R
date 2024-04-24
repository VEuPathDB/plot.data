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
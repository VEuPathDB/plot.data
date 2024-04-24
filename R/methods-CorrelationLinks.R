# Methods for Link and LinkList objects

# Accessors for fanciness
setGeneric("correlationCoef", function(object) standardGeneric("correlationCoef"))
setGeneric("correlationCoef<-", function(object, value) standardGeneric("correlationCoef<-"))
setGeneric("pValue", function(object) standardGeneric("pValue"))
setGeneric("pValue<-", function(object, value) standardGeneric("pValue<-"))

setMethod("correlationCoef", "CorrelationLink", function(object) object@correlationCoef)
setMethod("correlationCoef<-", "CorrelationLink", function(object, value) {object@correlationCoef <- value; validObject(object); object})
setMethod("pValue", "CorrelationLink", function(object) object@pValue)
setMethod("pValue<-", "CorrelationLink", function(object, value) {object@pValue <- value; validObject(object); object})

# a method to filter CorrelationLinkList by pValue and correlationCoef
#' Filter Correlation Links
#' 
#' This function filters CorrelationLinkList by pValue and correlationCoef
#' @param object CorrelationLinkList or CorrelationNetwork
#' @param correlationCoefThreshold threshold to filter edges by correlation coefficient. Default is NULL.
#' Any links with absolute correlation coefficients below this threshold will be removed.
#' @param pValueThreshold threshold to filter edges by p-value. Default is 0.05.
#' Any links with p-values above this threshold will be removed.
#' @param verbose boolean indicating if timed logging is desired
#' @return CorrelationLinkList or CorrelationNetwork
#' @export
#' @rdname pruneCorrelationLinks
setGeneric("pruneCorrelationLinks", 
function(
    object, 
    correlationCoefThreshold = NULL,
    pValueThreshold = 0.05, 
    verbose = c(TRUE, FALSE)
) {
    standardGeneric("pruneCorrelationLinks")
})

#' @rdname pruneCorrelationLinks
#' @aliases pruneCorrelationLinks,CorrelationLinkList-method
setMethod("pruneCorrelationLinks", "CorrelationLinkList", 
function(
    object, 
    correlationCoefThreshold = NULL,
    pValueThreshold = 0.05,
    verbose = c(TRUE, FALSE)
) {
    verbose <- veupathUtils::matchArg(verbose)

    if (!is.null(correlationCoefThreshold)) {
        correlationCoefs <- sapply(object, correlationCoef)
        newLinks <- object[abs(correlationCoefs) >= correlationCoefThreshold]
    }

    if (!is.null(pValueThreshold)) {
        pValues <- sapply(object, pValue)
        newLinks <- newLinks[pValues <= pValueThreshold]
    }

    if (verbose) {
        message("Removed ", length(object) - length(newLinks), " links")
    }

    validObject(newLinks)
    return(newLinks)
})
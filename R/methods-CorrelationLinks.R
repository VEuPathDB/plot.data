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

# Additional methods
#' @include methods-Links.R
setMethod("getSourceNodes", "CorrelationLinkList", function(object) lapply(object, function(x) source(x)))
setMethod("getTargetNodes", "CorrelationLinkList", function(object) lapply(object, function(x) target(x)))
setMethod("getWeights", "CorrelationLinkList", function(object) unlist(lapply(object, function(x) weight(x))))
setMethod("getColors", "CorrelationLinkList", function(object) unlist(lapply(object, function(x) color(x))))

setGeneric("getCorrelationCoefs", function(object) standardGeneric("getCorrelationCoefs"))
setMethod("getCorrelationCoefs", "CorrelationLinkList", function(object) unlist(lapply(object, function(x) correlationCoef(x))))
setGeneric("getPValues", function(object) standardGeneric("getPValues"))
setMethod("getPValues", "CorrelationLinkList", function(object) unlist(lapply(object, function(x) pValue(x))))

#' Filter Correlation Links
#' 
#' This function filters CorrelationLinkList by pValue and correlationCoef
#' @param object CorrelationLinkList or CorrelationNetwork
#' @param correlationCoefThreshold threshold to filter edges by correlation coefficient. Default is NULL.
#' Any links with absolute correlation coefficients below this threshold will be removed.
#' @param pValueThreshold threshold to filter edges by p-value. Default is NULL.
#' Any links with p-values above this threshold will be removed.
#' @param verbose boolean indicating if timed logging is desired
#' @return CorrelationLinkList or CorrelationNetwork
#' @export
#' @rdname pruneCorrelationLinks
setGeneric("pruneCorrelationLinks", 
function(
    object, 
    correlationCoefThreshold = NULL,
    pValueThreshold = NULL, 
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
    pValueThreshold = NULL,
    verbose = c(TRUE, FALSE)
) {
    verbose <- veupathUtils::matchArg(verbose)

    if (!is.null(correlationCoefThreshold)) {
        correlationCoefs <- sapply(object, correlationCoef)
        newLinks <- object[abs(correlationCoefs) >= correlationCoefThreshold]
    } else {
        newLinks <- object
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

toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' @export
setMethod(toJSONGeneric, signature("CorrelationLinkList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- veupathUtils::S4SimpleListToJSON(object, FALSE)

    if (named) tmp <- paste0('{"links":', tmp, "}")

    return(tmp)
})
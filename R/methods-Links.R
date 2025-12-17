# Methods for Link and LinkList objects

# Accessors for fanciness
setGeneric("source", function(object) standardGeneric("source"))
setGeneric("source<-", function(object, value) standardGeneric("source<-"))
setGeneric("target", function(object) standardGeneric("target"))
setGeneric("target<-", function(object, value) standardGeneric("target<-"))
setGeneric("weight", function(object) standardGeneric("weight"))
setGeneric("weight<-", function(object, value) standardGeneric("weight<-"))
setGeneric("color", function(object) standardGeneric("color"))
setGeneric("color<-", function(object, value) standardGeneric("color<-"))
setGeneric("isDirected", function(object) standardGeneric("isDirected"))
setGeneric("isDirected<-", function(object, value) standardGeneric("isDirected<-"))

setMethod("source", "Link", function(object) object@source)
setMethod("source<-", "Link", function(object, value) {object@source <- value; validObject(object); object})
setMethod("target", "Link", function(object) object@target)
setMethod("target<-", "Link", function(object, value) {object@target <- value; validObject(object); object})
setMethod("weight", "Link", function(object) object@weight)
setMethod("weight<-", "Link", function(object, value) {object@weight <- value; validObject(object); object})
setMethod("color", "Link", function(object) object@color)
setMethod("color<-", "Link", function(object, value) {object@color <- value; validObject(object); object})
setMethod("isDirected", "Link", function(object) object@isDirected)
setMethod("isDirected<-", "Link", function(object, value) {object@isDirected <- value; validObject(object); object})


# Additional methods
# Link properties such as color are returned as vectors, while grabbing particular nodes from the
# LinkList returns lists of nodes.
setGeneric("getSourceNodes", function(object) standardGeneric("getSourceNodes"))
setMethod("getSourceNodes", "LinkList", function(object) lapply(object, function(x) source(x)))
setGeneric("getTargetNodes", function(object) standardGeneric("getTargetNodes"))
setMethod("getTargetNodes", "LinkList", function(object) lapply(object, function(x) target(x)))
setGeneric("getWeights", function(object) standardGeneric("getWeights"))
setMethod("getWeights", "LinkList", function(object) unlist(lapply(object, function(x) weight(x))))
setGeneric("getColors", function(object) standardGeneric("getColors"))
setMethod("getColors", "LinkList", function(object) unlist(lapply(object, function(x) color(x))))


toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert Link object to JSON
#'
#' Converts a Link object to JSON
#' @param object A Link object
#' @param named boolean that declares if names should be included
setMethod(toJSONGeneric, "Link", function(object, named = c(FALSE, TRUE)) {
    named <- veupathUtils::matchArg(named)
    tmp <- character()

    tmp <- paste0('"source":', jsonlite::toJSON(jsonlite::unbox(id(source(object)))))
    tmp <- paste0(tmp, ',"target":', jsonlite::toJSON(jsonlite::unbox(id(target(object)))))
    if (!!length(weight(object))) tmp <- paste0(tmp, ',"weight":', jsonlite::toJSON(jsonlite::unbox(weight(object))))
    if (!!length(color(object))) tmp <- paste0(tmp, ',"color":', jsonlite::toJSON(jsonlite::unbox(color(object))))
    tmp <- paste0(tmp, ',"isDirected":', jsonlite::toJSON(jsonlite::unbox(isDirected(object))))

    tmp <- paste0('{', tmp, '}')
    if (named) {
        tmp <- paste0('{"link":', tmp, '}')
    }

    return(tmp)
})

setMethod(toJSONGeneric, signature("LinkList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- veupathUtils::S4SimpleListToJSON(object, FALSE)

    if (named) tmp <- paste0('{"links":', tmp, "}")

    return(tmp)
})
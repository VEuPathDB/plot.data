# Methods for Node and NodeList objects


# Accessors for fanciness
setGeneric("id", function(object) standardGeneric("id"))
setGeneric("id<-", function(object, value) standardGeneric("id<-"))
setGeneric("x", function(object) standardGeneric("x"))
setGeneric("x<-", function(object, value) standardGeneric("x<-"))
setGeneric("y", function(object) standardGeneric("y"))
setGeneric("y<-", function(object, value) standardGeneric("y<-"))

#' @include methods-Links.R
## Methods for Nodes
setMethod("id", "Node", function(object) id(object@id))
setMethod("x", "Node", function(object) object@x)
setMethod("y", "Node", function(object) object@y)
setMethod("color", "Node", function(object) object@color)
setMethod("weight", "Node", function(object) object@weight)
setMethod("id<-", "Node", function(object, value) {object@NodeId@id <- value; validObject(object); object})
setMethod("x<-", "Node", function(object, value) {object@x <- value; validObject(object); object})
setMethod("y<-", "Node", function(object, value) {object@y <- value; validObject(object); object})
setMethod("color<-", "Node", function(object, value) {object@color <- value; validObject(object); object})
setMethod("weight<-", "Node", function(object, value) {object@weight <- value; validObject(object); object})

## Methods for NodeId
setMethod("id", "NodeId", function(object) object@value)
setMethod("id<-", "NodeId", function(object, value) {object@id <- value; validObject(object); object})

## Methods for NodeLists
setGeneric("getNodeIds", function(object) standardGeneric("getNodeIds"))
setMethod("getNodeIds", "NodeList", function(object) unlist(lapply(as.list(object), id)))
setMethod("getWeights", "NodeList", function(object) unlist(lapply(as.list(object), weight)))
setMethod("getColors", "NodeList", function(object) unlist(lapply(as.list(object), color)))

## Methods for NodeIdList
setMethod("getNodeIds", "NodeIdList", function(object) unlist(lapply(as.list(object), id)))

## Methods for Partitions
setGeneric("getAllNodeIds", function(object) standardGeneric("getAllNodeIds"))
setMethod("getAllNodeIds", "Partitions", function(object) unlist(lapply(as.list(object), getNodeIds)))


toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert Node object to JSON
#' 
#' Converts a Node object to JSON
#' @param object A Node object
#' @param named boolean that declares if names should be included
#' @export
setMethod(toJSONGeneric, "Node", function(object, named = c(FALSE, TRUE)) {
    named <- veupathUtils::matchArg(named)
    tmp <- character()

    tmp <- paste0('"id":', jsonlite::toJSON(jsonlite::unbox(id(object))))
    if (!!length(x(object))) tmp <- paste0(tmp, ',"x":', jsonlite::toJSON(jsonlite::unbox(x(object))))
    if (!!length(y(object))) tmp <- paste0(tmp, ',"y":', jsonlite::toJSON(jsonlite::unbox(y(object))))
    if (!!length(color(object))) tmp <- paste0(tmp, ',"color":', jsonlite::toJSON(jsonlite::unbox(color(object))))
    if (!!length(weight(object))) tmp <- paste0(tmp, ',"weight":', jsonlite::toJSON(jsonlite::unbox(weight(object))))

    tmp <- paste0('{', tmp, '}')
    if (named) {
        tmp <- paste0('{"node":', tmp, '}')
    }

    return(tmp)
})

#' @export
setMethod(toJSONGeneric, signature("NodeList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- veupathUtils::S4SimpleListToJSON(object, FALSE)

    if (named) tmp <- paste0('{"nodes":', tmp, "}")

    return(tmp)
})
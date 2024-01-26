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
#alias for the id methods for NodeId
setGeneric("value", function(object) standardGeneric("value"))
setGeneric("value<-", function(object, value) standardGeneric("value<-"))
setMethod("value", "NodeId", function(object) id(object))
setMethod("value<-", "NodeId", function(object, value) {id(object) <- value; validObject(object); object})

## Methods for NodeLists
setGeneric("getNodeIds", function(object) standardGeneric("getNodeIds"))
setMethod("getNodeIds", "NodeList", function(object) unlist(lapply(as.list(object), id)))
setMethod("getWeights", "NodeList", function(object) unlist(lapply(as.list(object), weight)))
setMethod("getColors", "NodeList", function(object) unlist(lapply(as.list(object), color)))

## Methods for NodeIdList
setMethod("getNodeIds", "NodeIdList", function(object) unlist(lapply(as.list(object), id)))
setMethod("getNodeIds", "Network", function(object) getNodeIds(object@nodes))
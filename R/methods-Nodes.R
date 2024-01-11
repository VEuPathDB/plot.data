# Methods for Node and NodeList objects


# Accessors for fanciness
setGeneric("id", function(object) standardGeneric("id"))
setGeneric("id<-", function(object, value) standardGeneric("id<-"))
setGeneric("x", function(object) standardGeneric("x"))
setGeneric("x<-", function(object, value) standardGeneric("x<-"))
setGeneric("y", function(object) standardGeneric("y"))
setGeneric("y<-", function(object, value) standardGeneric("y<-"))
setGeneric("color", function(object) standardGeneric("color"))
setGeneric("color<-", function(object, value) standardGeneric("color<-"))
setGeneric("weight", function(object) standardGeneric("weight"))
setGeneric("weight<-", function(object, value) standardGeneric("weight<-"))

## Methods for Nodes
setMethod("id", "Node", function(object) object@NodeId@id)
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
setMethod("id", "NodeId", function(object) object@id)
setMethod("id<-", "NodeId", function(object, value) {object@id <- value; validObject(object); object})

## Methods for NodeLists
setGeneric("getNodeIds", function(object) standardGeneric("getNodeIds"))
setGeneric("getWeights", function(object) standardGeneric("getWeights"))
setGeneric("getColors", function(object) standardGeneric("getColors"))

setMethod("getNodeIds", "NodeList", function(object) unlist(lapply(object, function(x) id(x))))
setMethod("getWeights", "NodeList", function(object) unlist(lapply(object, function(x) weight(x))))
setMethod("getColors", "NodeList", function(object) unlist(lapply(object, function(x) color(x))))

## Methods for NodeIdList
setMethod("getNodeIds", "NodeIdList", function(object) unlist(lapply(object, function(x) id(x))))
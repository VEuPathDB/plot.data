# Methods for Node and NodeList objects


# Accessors for fanciness
setGeneric("id", function(object) standardGeneric("id"))
setGeneric("id<-", function(object, value) standardGeneric("id<-"))

setMethod("id", "Node", function(object) object@id)
setMethod("color", "Node", function(object) object@color)
setMethod("weight", "Node", function(object) object@weight)
setMethod("id<-", "Node", function(object, value) {object@id <- value; validObject(object); object})
setMethod("color<-", "Node", function(object, value) {object@color <- value; validObject(object); object})
setMethod("weight<-", "Node", function(object, value) {object@weight <- value; validObject(object); object})


## Methods for NodeLists
setGeneric("getNodeIds", function(object) standardGeneric("getNodeIds"))
setMethod("getNodeIds", "NodeList", function(object) unlist(lapply(object, function(x) id(x))))
setMethod("getWeights", "NodeList", function(object) unlist(lapply(object, function(x) weight(x))))
setMethod("getColors", "NodeList", function(object) unlist(lapply(object, function(x) color(x))))
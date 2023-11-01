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

setMethod("source", "Link", function(object) object@source)
setMethod("source<-", "Link", function(object, value) {object@source <- value; object})
setMethod("target", "Link", function(object) object@target)
setMethod("target<-", "Link", function(object, value) {object@target <- value; object})
setMethod("weight", "Link", function(object) object@weight)
setMethod("weight<-", "Link", function(object, value) {object@weight <- value; object})
setMethod("color", "Link", function(object) object@color)
setMethod("color<-", "Link", function(object, value) {object@color <- value; object})


# For LinkLists, let's return vectors of data from the nodes
setGeneric("getSourceNodes", function(object) standardGeneric("getSourceNodes"))
setMethod("getSourceNodes", "LinkList", function(object) lapply(object, function(x) source(x)))
setGeneric("getTargetNodes", function(object) standardGeneric("getTargetNodes"))
setMethod("getTargetNodes", "LinkList", function(object) lapply(object, function(x) target(x)))
setGeneric("getWeights", function(object) standardGeneric("getWeights"))
setMethod("getWeights", "LinkList", function(object) unlist(lapply(object, function(x) weight(x))))
setGeneric("getColors", function(object) standardGeneric("getColors"))
setMethod("getColors", "LinkList", function(object) unlist(lapply(object, function(x) color(x))))



# Remove redundant links?


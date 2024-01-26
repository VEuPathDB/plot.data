#' @include methods-Nodes.R
## Methods for Partitions
setGeneric("getAllNodeIds", function(object) standardGeneric("getAllNodeIds"))
setMethod("getAllNodeIds", "Partitions", function(object) unlist(lapply(as.list(object), getNodeIds)))
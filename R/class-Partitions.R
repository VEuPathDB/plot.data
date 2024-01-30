#' @include methods-Nodes.R
check_partitions <- function(object) {
  errors <- character()
  
  if (!!length(unlist(lapply(as.list(object), getNodeIds)))) {
    # Ensure that no node is in multiple partitions
    if (length(unlist(lapply(as.list(object), getNodeIds))) > data.table::uniqueN(getAllNodeIds(object))) {
      errors <- c(errors, 'Found a node in multiple partitions. Nodes can only exist in one partition.')
    }
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' Partitions
#' 
#' A class for representing partitions in a k-partite network
#' 
#' @name Partitions-class
#' @rdname Partitions-class
#' @export
setClass("Partitions", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "NodeIdList"
  ),
  validity = check_partitions
)

#' Create a Partition
#' 
#' An alias to NodeIdList
#' 
#' @name Partition-class
#' @rdname Partition-class
#' @export
Partition <- NodeIdList

#' Create Partitions
#' 
#' A list of Partition objects, each containing a list of node 
#' ids that belong to a single partition
#' 
#' @param partitions list of Partition (or NodeIdList) objects
#' @export 
#' @rdname Partitions
Partitions <- function(partitions = list()) {
  if (length(partitions) == 0) {
    return(new("Partitions", S4Vectors:::SimpleList(list())))
  }

  if (length(partitions) == 1 && !is.list(partitions)) {
    ## an edge case i suppose where we had a single partition w  a single node
    partitions <- list(Partition(partitions))
  }

  if (!is.list(partitions)) {
    stop('Partitions must be a list')
  }

  if (!all(unlist(lapply(partitions, inherits, "NodeIdList")))) {
    stop('Partitions must be a list of NodeIdList objects')
  }

  return(new("Partitions", S4Vectors:::SimpleList(partitions)))
}

#' Get Partition index of a Node
#' 
#' Given a list of partitions and a node id, return the index
#' of the partition that the node belongs to.
#' @param partitions Partitions
#' @param nodeId NodeId
#' @export
setGeneric("getPartitionIndex", function(partitions, nodeId) standardGeneric("getPartitionIndex"))
setMethod("getPartitionIndex", signature("Partitions", "NodeId"), function(partitions, nodeId) {
  return(which(unlist(lapply(partitions, function(x) id(nodeId) %in% getNodeIds(x)))))
})
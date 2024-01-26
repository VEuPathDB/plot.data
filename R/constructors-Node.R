#' @include class-Node.R
#' Create a Node Id
#' 
#' Because typing `NodeId(id = 'foo')` is annoying, this function is provided
#' to make things easier. Now you can do `NodeId('foo')`
#' 
#' @param value string a unique identifier for the node
#' @export 
NodeId <- function(value) {
  new("NodeId", value = value)
}


#' Create a NodeIdList
#' 
#' @param object Object containing list of node ids
#' @export 
setGeneric("NodeIdList", function(object, uniqueOnly = c(TRUE, FALSE)) standardGeneric("NodeIdList")) 

#' @export
setMethod("NodeIdList", "list", function(object, uniqueOnly = c(TRUE, FALSE)) {
  uniqueOnly <- veupathUtils::matchArg(uniqueOnly)
  nodeIds <- object

  if (length(nodeIds) == 0) {
    stop("nodeIds must not be empty")
  }

  if (!is.list(nodeIds)) {
    stop("nodeIds must be a list")
  }

  if (all(unlist(lapply(nodeIds, inherits, 'Node')))) {
    nodeIds <- lapply(nodeIds, id)
    if (uniqueOnly) {
      nodeIds <- unique(nodeIds)
    }
    nodeIds <- lapply(nodeIds, NodeId)
  } else if (all(unlist(lapply(nodeIds, inherits, 'character')))) {
    if (uniqueOnly) {
      nodeIds <- unique(nodeIds)
    }
    nodeIds <- lapply(nodeIds, NodeId)
  } else if (!all(unlist(lapply(nodeIds, inherits, 'NodeId')))) {
    stop("nodeIds must be a list of Node, NodeId or character objects")
  }

  return(new("NodeIdList", S4Vectors::SimpleList(nodeIds)))
})

#' @export 
setMethod("NodeIdList", "NodeList", function(object, uniqueOnly = c(TRUE, FALSE)) {
  return(NodeIdList(getNodeIds(object), uniqueOnly = uniqueOnly))
})

#' @export
setMethod("NodeIdList", "character", function(object, uniqueOnly = c(TRUE, FALSE)) {
  uniqueOnly <- veupathUtils::matchArg(uniqueOnly)

  if (length(object) == 0) {
    stop("nodeIds must not be empty")
  }

  if (uniqueOnly) {
    object <- unique(object)
  }

  return(new("NodeIdList", S4Vectors::SimpleList(lapply(object, NodeId))))
}) 

#' @export 
setMethod("NodeIdList", "data.frame", function(object, uniqueOnly = c(TRUE, FALSE)) {  
  if (!isValidEdgeList(object)) {
    stop(paste(errors, collapse = '\n'))
  }

  return(NodeIdList(c(object$source, object$target), uniqueOnly = uniqueOnly))
})

#' @export 
setMethod("NodeIdList", "missing", function(object, uniqueOnly = c(TRUE, FALSE)) {
  return(new("NodeIdList"))
})

#' @export 
setMethod("NodeIdList", "Node", function(object, uniqueOnly = c(TRUE, FALSE)) {
  return(NodeIdList(list(object)))
})


#' Create a Node
#' 
#' @param id string a unique identifier for the node
#' @param x numeric value indicating the x coordinate of the node. Optional.
#' @param y numeric value indicating the y coordinate of the node. Optional.
#' @param color string or numeric that determines the color of the node. Optional.
#' @param weight numeric value associated with the node, such as timestamp or other node-associated data. Optional.
#' @export
setGeneric("Node", function(id, x = numeric(), y = numeric(), color = NULL, weight = NULL) standardGeneric("Node"), signature = c("id"))

#' @export
setMethod("Node", "numeric", function(id, x = numeric(), y = numeric(), color = NULL, weight = NULL) {
  new("Node", id = NodeId(as.character(id)), x = x, y = y, color = color, weight = weight)
})

#' @export
setMethod("Node", "character", function(id, x = numeric(), y = numeric(), color = NULL, weight = NULL) {
  new("Node", id = NodeId(id), x = x, y = y, color = color, weight = weight)
})

#' @export
setMethod("Node", "NodeId", function(id, x = numeric(), y = numeric(), color = NULL, weight = NULL) {
  new("Node", id = id, x = x, y = y, color = color, weight = weight)
})

#' @export 
setMethod("Node", "missing", function(id, x = numeric(), y = numeric(), color = NULL, weight = NULL) {
  new("Node", id = NodeId(generate_node_id(1)), x = x, y = y, color = color, weight = weight)
})


#' @include utils.R
#' Generate a NodeList
#' 
#' Generate a NodeList from an edgeList
#' @param object Object containing data to be converted to a NodeList
#' @return NodeList
#' @export
#' @examples
#' NodeList(data.frame(source='a',target='b'))
setGeneric("NodeList", function(object) standardGeneric("NodeList"))

#' @export
setMethod("NodeList", "data.frame", function(object = data.frame(source=character(),target=character())) {
  if (!isValidEdgeList(object)) {
    stop(paste(errors, collapse = '\n'))
  }
  
  allNodeIds <- unique(c(object$source, object$target))
  nodesList <- lapply(allNodeIds, Node)
  new("NodeList", nodesList)
})

#' @export
setMethod("NodeList", "missing", function(object) {
  new("NodeList")
})

#' @export 
setMethod("NodeList", "SimpleList", function(object) {
  new("NodeList", object)
})

#' @export 
setMethod("NodeList", "list", function(object) {
  new("NodeList", object)
})
check_node <- function(object) {

  errors <- character()

  # Node color must be a string or number
  if (!is.null(object@color) & !is.character(object@color) & !is.numeric(object@color)) {
    errors <- c(errors, "Node color must be a string or number")
  }

  # If Node has x it must have y and vice versa
  if (!is.null(object@x) & is.null(object@y)) {
    errors <- c(errors, "If Node has x it must have y and vice versa")
  } else if (is.null(object@x) & !is.null(object@y)) {
    errors <- c(errors, "If Node has y it must have x and vice versa")
  }

  # Node weight must be a number
  if (!is.null(object@weight) & !is.numeric(object@weight)) {
    errors <- c(errors, "Node weight must be a number")
  }

  return(if (length(errors) == 0) TRUE else errors)
}


#' Node
#' 
#' A class for representing nodes in a network
#' 
#' @slot id string a unique identifier for the node
#' @slot x numeric value indicating the x coordinate of the node. Optional.
#' @slot y numeric value indicating the y coordinate of the node. Optional.
#' @slot color string or numeric that determines the color of the node. Optional.
#' @slot weight numeric value associated with the node, such as timestamp or other node-associated data. Optional.
#' 
#' @name Node-class
#' @rdname Node-class
#' @export
Node <- setClass("Node", 
  representation(
    id = NodeId,
    x = "numeric",
    y = "numeric",
    color = "ANY",
    weight = "ANY"
  ),
  prototype = prototype(
    id = character()
  ),
  validity = check_node
)

check_node_id <- function(object) {
  errors <- character()

  # node id must not be empty
  if (length(object@id) == 0) {
    errors <- c(errors, "Node id must not be empty")
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' A Node Id
#' 
#' A class for representing node ids
#' 
#' @name NodeId-class
#' @rdname NodeId-class
#' @export
NodeId <- setClass("NodeId", 
  representation(
    id = "character"
  ),
  prototype = prototype(
    id = character()
  ),
  validity = check_node_id
)

check_node_id_list <- function(object) {
  errors <- character()

   # make sure all ids are unique
  if (length(unique(unlist(lapply(object, id)))) != length(unlist(lapply(object, id)))) {
    errors <- c(errors, "Node ids must be unique")
  }

  return(if (length(errors) == 0) TRUE else errors)
}


#' A Node Id List
#' 
#' A class for representing node id lists
#' 
#' @name NodeIdList-class
#' @rdname NodeIdList-class
#' @export
NodeIdList <- setClass("NodeIdList", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "NodeId"
  ),
  validity = check_node_id_list
)


check_node_list <- function(object) {

  errors <- character()

  if (any(unlist(lapply(object, function(x) {!is.null(color(x))})))) {
    # If one node has a color, all must have colors
    if (!all(unlist(lapply(object, function(x) {!is.null(color(x))})))) {
      errors <- c(errors, "If one node has a color, all nodes must have a color")
    }

    # Node colors must be all the same class
    if (length(unique(unlist(lapply(object, function(x) {class(color(x))})))) > 1) {
      errors <- c(errors, "Node colors must be all the same class")
    }
  }

  # If one node has a weight, all must have weights
  if (any(unlist(lapply(object, function(x) {!is.null(weight(x))})))) {
    if (!all(unlist(lapply(object, function(x) {!is.null(weight(x))})))) {
      errors <- c(errors, "If one node has a weight, all nodes must have a weight")
    }
  }


  return(if (length(errors) == 0) TRUE else errors)
}


#' NodeList
#' 
#' A class for representing a list of nodes.
#' 
#' @name NodeList-class
#' @rdname NodeList-class
#' @importFrom S4Vectors SimpleList
#' @export
NodeList <- setClass("NodeList", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "Node"
  ),
  validity = check_node_list
)



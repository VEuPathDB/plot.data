
#' Node
#' 
#' A class for representing nodes in a network
#' 
#' @slot id string a unique identifier for the node
#' @slot color string or numeric that determines the color of the node
#' @slot weight numeric value associated with the node, such as timestamp or other node-associated data. Optional.
#' 
#' @name Node-class
#' @rdname Node-class
#' @export
Node <- setClass("Node", 
  representation(
    id = "character",
    color = "character",
    weight = "numeric"
  ),
  prototype = prototype(
    id = character()
  )
)


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
  )
)





check_link <- function(object) {

  errors <- character()

  # Link color must be a string or number
  if (!is.null(object@color) & !is.character(object@color) & !is.numeric(object@color)) {
    errors <- c(errors, "Link color must be a string or number that represents a color or can be mapped to a color.")
  }

  return(if (length(errors) == 0) TRUE else errors) 
}


#' Link
#' 
#' Represent one singular link. A link has a source, and a target. It may be directed or undirected.
#' It may have an associated weight, color, timestamp, or label (coming soon!)
#' 
#' @name Link-class
#' @rdname Link-class
#' @include class-Node.R
#' @export
setClass("Link", 
  representation(
    source = "Node",
    target = "Node",
    weight = "numeric",
    color = 'ANY',
    isDirected = "logical"
    # label = "character" # coming soon
  ),
  prototype = prototype(
    source = new("Node"),
    target = new("Node"),
    weight = 1,
    isDirected = FALSE
  ),
  validity = check_link
)

setMethod("initialize", "Link", function(
  .Object, 
  source = character(), 
  target = character(), 
  weight = 1, 
  color = NULL, 
  isDirected = FALSE,
  ...
) { 
  .Object <- callNextMethod(.Object, ...)
  .Object@source <- Node(source)
  .Object@target <- Node(target)
  .Object@weight <- weight
  .Object@color <- color
  .Object@isDirected <- isDirected
              
  .Object
})

setMethod("initialize", "Link", function(
  .Object, 
  source = Node(), 
  target = Node(), 
  weight = 1, 
  color = NULL, 
  isDirected = FALSE, 
  ...
) {

  .Object <- callNextMethod(.Object, ...)
  .Object@source <- source
  .Object@target <- target
  .Object@weight <- weight
  .Object@color <- color
  .Object@isDirected <- isDirected
              
  .Object
})

check_link_list <- function(object) {

  errors <- character()
  
  if (any(unlist(lapply(object, function(x) {!is.null(color(x))})))) {
    # If one link has a color, all must have colors
    if (!all(unlist(lapply(object, function(x) {!is.null(color(x))})))) {
      errors <- c(errors, "If one link has a color, all links must have a color")
    }
    
    # Link colors must be all the same class
    if (length(unique(unlist(lapply(object, function(x) {class(color(x))})))) > 1) {
      errors <- c(errors, "Link colors must be all the same class")
    }
  }


  # If one link has a weight, all must have weights
  if (any(unlist(lapply(object, function(x) {!is.null(weight(x))})))) {
    if (!all(unlist(lapply(object, function(x) {!is.null(weight(x))})))) {
      errors <- c(errors, "If one link has a weight, all links must have a weight")
    }
  }


  return(if (length(errors) == 0) TRUE else errors) 

}

#' Link List
#' 
#' A class for representing links in a network
#' 
#' @name LinkList-class
#' @rdname LinkList-class
#' @importFrom S4Vectors SimpleList
#' @export
setClass("LinkList", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "Link"
  ),
  validity = check_link_list
)

setMethod("initialize", "LinkList", function(
  .Object, 
  edgeList = data.frame(), 
  ...
) {
  if (!isValidEdgeList(edgeList)) {
    stop(paste(errors, collapse = '\n'))
  }
        
  .Object <- callNextMethod(.Object, ...)  
  .Object <- S4Vectors::SimpleList(lapply(edgeList, Link))
              
  .Object
})
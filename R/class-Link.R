

check_link <- function(object) {

  errors <- character()

  # Link color must be a string or number
  if (!is.null(object@color) & !is.character(object@color) & !is.numeric(object@color)) {
    errors <- c(errors, "Link color must be a string or number that represents a color or can be mapped to a color.")
  }

  #dont allow self-links for now
  if (id(object@source) == id(object@target)) {
    errors <- c(errors, "Links cannot be self-links. They must have a different source and target.")
  }

  return(if (length(errors) == 0) TRUE else errors) 
}

# TODO pretty sure source and target here should be NodeId rather than Node
# TODO consider its the same for Partitions
#' Link
#' 
#' Represent one singular link. A link has a source, and a target. It may be directed or undirected.
#' It may have an associated weight, color, timestamp, or label (coming soon!)
#' 
#' @name Link-class
#' @rdname Link-class
#' @include class-Node.R
#' @include constructors-Node.R
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
    source = Node(),
    target = Node(),
    weight = 1,
    color = NULL,
    isDirected = FALSE
  ),
  validity = check_link
)

#' Link constructor
#' 
#' @param source The source node
#' @param target The target node
#' @param weight The weight of the link
#' @param color The color of the link
#' @param isDirected Whether the link is directed
#' @export
setGeneric("Link", function(source, target, weight = 1, color = NULL, isDirected = FALSE) standardGeneric("Link"), signature = c("source", "target"))

#' @export
setMethod("Link", c("Node", "Node"), function(source, target, weight = 1, color = NULL, isDirected = FALSE) {
  new("Link", source = source, target = target, weight = weight, color = color, isDirected = isDirected)
})

#' @export
setMethod("Link", c("character", "character"), function(source, target, weight = 1, color = NULL, isDirected = FALSE) {
  Link(source = Node(source), target = Node(target), weight = weight, color = color, isDirected = isDirected)
})

#' @export
setMethod("Link", c("numeric", "numeric"), function(source, target, weight = 1, color = NULL, isDirected = FALSE) {
  Link(source = Node(source), target = Node(target), weight = weight, color = color, isDirected = isDirected)
})

#' @export
setMethod("Link", c("NodeId", "NodeId"), function(source, target, weight = 1, color = NULL, isDirected = FALSE) {
  Link(source = Node(source), target = Node(target), weight = weight, color = color, isDirected = isDirected)
})

#' @export
setMethod("Link", c("missing", "missing"), function(source, target, weight = 1, color = NULL, isDirected = FALSE) {
  new("Link")
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

#' Generate a LinkList
#' 
#' Generate a LinkList from an edgeList
#' @param object Object containing data to be converted to a LinkList. Could be a SimpleList of Links or a data.frame
#' with columns source, target, and optionally weight and color.
#' @param linkColorScheme Either 'none' or 'posneg'. If 'posneg', the link color will be based on the sign of the weight.
#' @return LinkList
#' @export
#' @examples
#' LinkList(data.frame(source='a',target='b'))
setGeneric("LinkList", function(object, linkColorScheme = c('none', 'posneg')) standardGeneric("LinkList"), signature = c("object"))

#' @export
setMethod("LinkList", "data.frame", function(object = data.frame(source=character(),target=character()), linkColorScheme = c('none', 'posneg')) {
  linkColorScheme <- veupathUtils::matchArg(linkColorScheme)
  
  if (!inherits(isValidEdgeList(object), "logical")) {
    stop(paste("Invalid edgeList:", isValidEdgeList(object), collapse = '\n'))
  }

  if (nrow(object) == 0) {
    new("LinkList")
  }

  makeLink <- function(x, linkColorScheme) {
    source <- unname(x['source'])
    target <- unname(x['target'])
    weight <- as.numeric(unname(x['weight']))
    weight <- ifelse(is.na(weight), 1, weight)
    isDirected <- unname(x['isDirected'])
    isDirected <- ifelse(is.na(isDirected), FALSE, isDirected)

    if (linkColorScheme == 'posneg') {
      if (weight < 0) {
        color <- -1
      } else if (weight > 0) {
        color <- 1
      } else {
        color <- 0
      }
    } else {
      color <- NA_character_
    }
    
    if (is.na(color)) {
      link <- Link(source, target, weight, NULL, isDirected)
    } else {
      link <- Link(source, target, weight, color, isDirected)
    }

    return(link)
  }

  linkList <- apply(object, 1, makeLink, linkColorScheme)
  new("LinkList", linkList)
})

#' @export
setMethod("LinkList", "missing", function(object) {
  new("LinkList")
})

#' @export 
setMethod("LinkList", "SimpleList", function(object) {
  new("LinkList", object)
})

#' @export 
setMethod("LinkList", "list", function(object) {
  new("LinkList", object)
})
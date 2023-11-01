

check_link <- function(object) {

  errors <- character()

  return(if (length(errors) == 0) TRUE else errors) 
}


# i hate this
#' Link
#' 
#' Represent one singular link. A link has a source, and a target. It may be directed or undirected.
#' It may have an associated weight, color, timestamp, or label (coming soon!)
#' 
#' @name Link-class
#' @rdname Link-class
#' @include class-Node.R
#' @export
Link <- setClass("Link", 
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


check_link_list <- function(object) {

  errors <- character()
  
  # If one link has a color, all must have colors

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
LinkList <- setClass("LinkList", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "Link"
  ),
  validity = check_link_list
)


### To Do:
## - LinkList needs a method assignLinkColors() to assign colors to links. Could take LinkList or Network i guess
## - i think all the coloring should go in the Network. Coloring edges or nodes could depend on edges or nodes, so ...
## - validation


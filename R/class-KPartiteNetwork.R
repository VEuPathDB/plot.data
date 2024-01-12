check_kpartite_network <- function(object) {

  errors <- character()

  # Check that all nodes are in at least one of the columns
  if (!all(getNodeIds(object@nodes) %in% getAllNodeIds(object@partitions))) {
    errors <- c(errors, 'Found a node that is not in any partition. All nodes must be assigned to a partition.')
  }
  if (!all(getAllNodeIds(object@partitions) %in% getNodeIds(object@nodes))) {
    errors <- c(errors, 'Found an id in a partition that is not in the nodes list. All partitions must must include ids in the nodes list.')
  }

  # Check that linkColorScheme is one of the accepted values
  if (!object@linkColorScheme %in% c('none', 'posneg')) {
    errors <- c(errors, 'linkColorScheme must be one of "none" or "posneg"')
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' k-Partite Network 
#' 
#' The k-partite network class represents data in the form of a network with k distinct groups of nodes
#' in which nodes connect only with nodes from the other groups. In other words, there are only inter-group
#' links, no intra-group links. These k groups are commonly called partitions.
#' k-partite networks can have any property of a regular network, but they also designate the node ids
#' that belong to each partition (group).
#' 
#' @slot links LinkList object defining the links in the network.
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' @slot partitions list of node ids that belong to each partition
#' 
#' @name KPartiteNetwork-class
#' @rdname KPartiteNetwork-class
#' @include class-network.R
#' @export 
KPartiteNetwork <- setClass("KPartiteNetwork", 
  contains = "Network",
  representation(
    partitions = Partitions
  ), prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none',
    partitions = Partitions()
  ),
  validity = check_kpartite_network
)

check_partitions <- function(object) {
  errors <- character()
  
  # Ensure that no node is in multiple partitions
  if (getAllNodeIds(object) > unique(getAllNodeIds(object))) {
    errors <- c(errors, 'Found a node in multiple partitions. Nodes can only exist in one partition.')
  }

  return(if (length(errors) == 0) TRUE else errors)
}

Partitions <- setClass("Partitions", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "NodeIdList"
  ),
  validity = check_partitions
)
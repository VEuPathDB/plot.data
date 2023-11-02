check_bipartite_network <- function(object) {

  errors <- character()

  # Ensure that no node is in both columns
  if (length(intersect(object@column1NodeIDs, object@column2NodeIDs)) > 0) {
    errors <- c(errors, 'Bipartite networks cannot have nodes in both columns.')
  }

  # Check that all nodes are in at least one of the columns
  if (!all(getNodeIds(object@nodes) %in% c(object@column1NodeIDs, object@column2NodeIDs))) {
    errors <- c(errors, 'Found a node with a link that does not belong to either column. All nodes must exist in exactly one of the two columns in a bipartite network.')
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' Bipartite Network 
#' 
#' The bipartite network class represents data in the form of a network with two distinct groups of nodes
#' in which nodes connect only with nodes from the other group. In other words, there are only inter-group
#' links, no intra-group links. The two groups of nodes are commonly displayed as two columns of nodes.
#' Bipartite networks can have any property of a regular network, but they also designate the node ids
#' that belong to each column (group).
#' 
#' @slot links LinkList object defining the links in the network.
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' @slot column1NodeIDs character vector listing the IDs of the nodes in the first column
#' @slot column2NodeIDs character vector listing the IDs of the nodes in the second column
#' 
#' @name BipartiteNetwork-class
#' @rdname BipartiteNetwork-class
#' @include class-network.R
#' @export 
BipartiteNetwork <- setClass("BipartiteNetwork", 
  contains = "Network",
  representation(
    column1NodeIDs = "character",
    column2NodeIDs = "character"
  ), prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none',
    column1NodeIDs = character(),
    column2NodeIDs = character()
  ),
  validity = check_bipartite_network
)
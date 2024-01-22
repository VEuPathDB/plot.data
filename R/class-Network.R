# Network class
check_network <- function(object) {

  errors <- character()

  # Check that all nodes in links are in nodes
  nodesInLinks <- NodeList(union(getSourceNodes(object@links), getTargetNodes(object@links))) # may become a method later if i find i use it elsewhere
  if (!all(getNodeIds(nodesInLinks) %in% getNodeIds(object@nodes))) {
    errors <- c(errors, 'Found a node in a link that is not in the node list. All nodes in links must also be in nodes.')
  }

  # Check that linkColorScheme is one of the accepted values
  if (!object@linkColorScheme %in% c('none', 'posneg')) {
    errors <- c(errors, 'linkColorScheme must be one of "none" or "posneg"')
  }

  # Check that there are no duplicate nodes
  if (data.table::uniqueN(getNodeIds(object@nodes)) < length(getNodeIds(object@nodes))) {
    errors <- c(errors, 'Duplicate node ids found. Node ids must be unique.')
  }


  return(if (length(errors) == 0) TRUE else errors)
}


#' Network
#' 
#' A class for representing networks. A network is composed of nodes and links (edges, connections, etc.). A link is represented
#' as a pair of nodes, with optional attributes such as weight (see Link). To represent a network, we need both the list of links in the network and a list of nodes
#' in case some nodes have no links. A network can also have properties such as directedness, levels, colors, etc. (coming soon).
#' 
#' @slot links LinkList object defining the links in the network.
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' @slot variableMapping veupathUtils::VariableMetadataList object defining the variable mappings in the network.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' 
#' @name Network-class
#' @rdname Network-class
#' @include class-Link.R
#' @export
Network <- setClass("Network", 
  representation(
    links = "LinkList",
    nodes = "NodeList",
    linkColorScheme = "character",
    variableMapping = "VariableMetadataList"
  ), prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none',
    variableMapping = VariableMetadataList()
  ),
  validity = check_network
)
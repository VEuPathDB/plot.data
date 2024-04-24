# Network class
check_network <- function(object) {

  errors <- character()

  # Check that all nodes in links are in nodes
  nodesInLinks <- NodeIdList(union(getSourceNodes(object@links), getTargetNodes(object@links))) # may become a method later if i find i use it elsewhere
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

# an abstract/ non-implementable class for other types of networks to subclass
setClass("BaseNetwork",
  slots = list(
    nodes = "NodeList",
    linkColorScheme = "character",
    variableMapping = "VariableMetadataList"
  ),
  contains = "VIRTUAL"
)

#' Network
#' 
#' A class for representing networks. A network is composed of nodes and links (edges, connections, etc.). A link is represented
#' as a pair of nodes, with optional attributes such as weight (see Link). To represent a network, we need both the list of links in the network and a list of nodes
#' in case some nodes have no links. A network can also have properties such as directedness, levels, colors, etc. (coming soon).
#' 
#' @slot links LinkList object defining the links in the network.
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' In the case of 'posneg', the links color slot will be set to 1 if the link is positive, and -1 if the link is negative.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' @slot variableMapping veupathUtils::VariableMetadataList object defining the variable mappings in the network. 
#' @name Network-class
#' @rdname Network-class
#' @include class-Link.R
#' @export
setClass("Network",
  contains = "BaseNetwork",
  slots = list(
    links = "LinkList"
  ),
  prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none',
    variableMapping = VariableMetadataList()
  ),
  validity = check_network
)

#' @include utils.R
#' Generate a Network 
#' 
#' Generate a Network from a LinkList and NodeList, or from a 
#' data.frame with columns 'source' and 'target', and optionally 'weight' and 'color'.
#' @param links LinkList
#' @param nodes NodeList
#' @param object Object containing data to be converted to a Network
#' @param linkColorScheme string defining the type of coloring scheme the links follow. 
#' Options are 'none' (default) and 'posneg'.
#' @param layout string defining the layout of the network. Options are 'force', 'circle', 
#' and 'nicely' which are implemented in igraph. Default is 'nicely'.
#' @param variables VariableMetadataList
#' @return Network
#' @export
#' @examples
#' Network(data.frame(source='a',target='b'))
setGeneric("Network", 
  function(
    object,
    links,
    nodes, 
    linkColorScheme = 'none', 
    variables = VariableMetadataList(), 
    ...
  ) standardGeneric("Network"),
  signature = c("object", "links", "nodes")
)

#' @export
setMethod("Network", signature("missing", "LinkList", "NodeList"), function(
  object,
  links, 
  nodes,
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("Network", links=links, nodes=nodes, linkColorScheme=linkColorScheme, variableMapping=variables)
})

#' @export  
setMethod("Network", signature("data.frame", "missing", "missing"), function(
  object = data.frame(source=character(),target=character()), 
  links, 
  nodes,
  linkColorScheme = 'none', 
  layout = c("nicely", "force", "circle"),
  variables = VariableMetadataList(), 
  ...
) {
  layout <- veupathUtils::matchArg(layout)
  new("Network", links=LinkList(object, linkColorScheme), nodes=NodeList(object, layout), linkColorScheme=linkColorScheme, variableMapping=variables)
})

#' @export 
setMethod("Network", signature("missing", "missing", "missing"), function(
  object, 
  links, 
  nodes,
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("Network")
})
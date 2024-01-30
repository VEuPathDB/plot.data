  isLinkWithinPartition <- function(link, partitions) {
    if (!inherits(link, "Link")) {
      stop('link must be a Link object')
    }
    if (!inherits(partitions, "Partitions")) {
      stop('partitions must be a Partitions object')
    }
    
    source <- source(link)
    target <- target(link)

    if (is.null(source) || is.null(target)) {
      return(FALSE)
    }

    internalLink <- FALSE
    if (getPartitionIndex(partitions, source) == getPartitionIndex(partitions, target)) {
      internalLink <- TRUE
    }

    return(internalLink)
  }

#' @include class-Partitions.R
check_kpartite_network <- function(object) {

  errors <- character()

  # Check we have at least two partitions
  if (length(object@partitions) < 2) {
    errors <- c(errors, 'k-partite networks must have at least two partitions.')
  }

  # Check that all nodes are in at least one of the partitions
  if (!all(getNodeIds(object@nodes) %in% getAllNodeIds(object@partitions))) {
    errors <- c(errors, 'Found a node that is not in any partition. All nodes must be assigned to a partition.')
  } else if (any(sapply(getLinks(object), isLinkWithinPartition, object@partitions))) {
    # Check that there are no links connecting nodes within a partition, only across the different partitions
    # this check wont work if a node is missing from a partition
    errors <- c(errors, 'Found a link between nodes in the same partition. Links between nodes in the same partition are not allowed.')
  }
  if (!all(getAllNodeIds(object@partitions) %in% getNodeIds(object@nodes))) {
    errors <- c(errors, 'Found an node id in a partition that is not in the nodes list. Node IDs must be consistent between partitions and nodes slots.')
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
#' @include class-Network.R
#' @export 
KPartiteNetwork <- setClass("KPartiteNetwork", 
  contains = "Network",
  slots = c(
    partitions = "Partitions"
  ),
  prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none',
    variableMapping = VariableMetadataList(),
    partitions = Partitions()
  ),
  validity = check_kpartite_network
)

#' @include utils.R
#' Generate a K-Partite Network 
#' 
#' Generate a K-Partite Network from a LinkList and NodeList, or from a data.frame
#' @param object Object containing data to be converted to a Network
#' @param links LinkList
#' @param nodes NodeList
#' @param partitions Partitions
#' @param linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' @param variables VariableMetadataList
#' @return KPartiteNetwork
#' @export
#' @examples
#' KPartiteNetwork(data.frame(source='a',target='b'))
setGeneric("KPartiteNetwork", 
  function(
    object,
    links,
    nodes,
    partitions = Partitions(),
    linkColorScheme = 'none', 
    variables = VariableMetadataList(), 
    ...
  ) standardGeneric("KPartiteNetwork"),
  signature = c("object", "links", "nodes")
)

#' @export
setMethod("KPartiteNetwork", signature("missing", "LinkList", "NodeList"), function(
  object,
  links, 
  nodes,
  partitions = Partitions(),
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("KPartiteNetwork", links=links, nodes=nodes, partitions=partitions, linkColorScheme=linkColorScheme, variableMapping=variables)
})

#' @export  
setMethod("KPartiteNetwork", signature("data.frame", "missing", "missing"), function(
  object = data.frame(source=character(),target=character()),
  links, 
  nodes,
  partitions = Partitions(),
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("KPartiteNetwork", links=LinkList(object), nodes=NodeList(object), partitions=partitions, linkColorScheme=linkColorScheme, variableMapping=variables)
})

#' @export
setMethod("KPartiteNetwork", signature("Network", "missing", "missing"), function(
  object,
  links, 
  nodes,
  partitions = Partitions(),
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  nodes <- object@nodes
  links <- object@links

  new("KPartiteNetwork", links=links, nodes=nodes, partitions=partitions, linkColorScheme=linkColorScheme, variableMapping=variables)
})

#' @export 
setMethod("KPartiteNetwork", signature("missing", "missing", "missing"), function(
  object,
  links, 
  nodes,
  partitions = Partitions(),
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("KPartiteNetwork")
})
# Network class
check_correlation_network <- function(object) {

  errors <- character()

  #TODO
  # Check we have thresholds defined

  # Check all links meet thresholding requirements

  return(if (length(errors) == 0) TRUE else errors)
}

#' Correlation Network
#' 
#' A class for representing networks of pairwise correlations. A network is composed of nodes and links (edges, connections, etc.). 
#' A link is represented as a pair of nodes, with attributes such as correlationCoef and pValue (see Link). To represent a network, 
#' we need both the list of links in the network and a list of nodes in case some nodes have no links. A network can also have 
#' properties such as directedness, levels, colors, etc. (coming soon).
#' 
#' @slot links CorrelationLinkList object defining the links in the network.
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' In the case of 'posneg', the links color slot will be set to 1 if the link is positive, and -1 if the link is negative.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' @slot variableMapping veupathUtils::VariableMetadataList object defining the variable mappings in the network.
#' @slot correlationCoefThreshold numeric defining the correlation coefficient threshold for filtering links. Default is NA (no filtering).
#' Any link with an absolute correlation coefficient below this threshold will be filtered out.
#' @slot pValueThreshold numeric defining the p-value threshold for filtering links. Default is 0.05.
#' Any link with an p-value above this threshold will be filtered out.
#' 
#' @name CorrelationNetwork-class
#' @rdname CorrelationNetwork-class
#' @include class-Network.R
#' @export
setClass("CorrelationNetwork", 
  contains = "BaseNetwork",
  slots = list(
    links = "CorrelationLinkList",
    correlationCoefThreshold = "numeric",
    pValueThreshold = "numeric"
  ),
  prototype = prototype(
    links = CorrelationLinkList(),
    nodes = NodeList(),
    linkColorScheme = 'posneg', #change default from base Network's 'none'
    correlationCoefThreshold = NA_real_,
    pValueThreshold = 0.05,
    variableMapping = VariableMetadataList()
  ),
  validity = check_correlation_network
)

#' @include utils.R
#' Generate a Correlation Network 
#' 
#' Generate a CorrelationNetwork from a CorrelationLinkList and NodeList, or from a 
#' data.frame with columns 'source', 'target', 'correlationCoef', 'pValue'.
#' @param links CorrelationLinkList
#' @param nodes NodeList
#' @param object Object containing data to be converted to a Network
#' @param correlationCoefThreshold numeric defining the correlation coefficient threshold for filtering links. Default is NULL (no filtering).
#' Any link with an absolute correlation coefficient below this threshold will be filtered out.
#' @param pValueThreshold numeric defining the p-value threshold for filtering links. Default is 0.05.
#' Any link with an p-value above this threshold will be filtered out.
#' @param linkColorScheme string defining the type of coloring scheme the links follow. 
#' Options are 'none' and 'posneg' (default).
#' @param layout string defining the layout of the network. Options are 'force', 'circle', 
#' and 'nicely' which are implemented in igraph. Default is 'nicely'.
#' @param variables VariableMetadataList
#' @return CorrelationNetwork
#' @export
#' @examples
#' CorrelationNetwork(data.frame(source='a', target='b', correlationCoef=0.5, pValue=0.05))
#' @rdname CorrelationNetwork
setGeneric("CorrelationNetwork",
  function(
    object,
    links,
    nodes,
    correlationCoefThreshold = NULL,
    pValueThreshold = 0.05,
    linkColorScheme = 'posneg',
    variables = VariableMetadataList(),
    ...
  ) standardGeneric("CorrelationNetwork"),
  signature = c("object", "links", "nodes")
)

#' @rdname CorrelationNetwork
#' @aliases CorrelationNetwork,missing,CorrelationLinkList,NodeList
setMethod("CorrelationNetwork", signature("missing", "CorrelationLinkList", "NodeList"), function(
  object,
  links,
  nodes,
  correlationCoefThreshold = NULL,
  pValueThreshold = 0.05,
  linkColorScheme = 'posneg', 
  variables = VariableMetadataList(),
  ...
) {
  links <- pruneCorrelationLinks(links, correlationCoefThreshold, pValueThreshold)
  
  new("CorrelationNetwork", 
    links=links, 
    nodes=nodes, 
    linkColorScheme=linkColorScheme, 
    variableMapping=variables,
    correlationCoefThreshold=correlationCoefThreshold,
    pValueThreshold=pValueThreshold
    )
})

#' @rdname CorrelationNetwork
#' @aliases CorrelationNetwork,data.frame,missing,missing
setMethod("CorrelationNetwork", signature("data.frame", "missing", "missing"), function(
  object = data.frame(source=character(),target=character()), 
  links, 
  nodes,
  correlationCoefThreshold = NULL,
  pValueThreshold = 0.05,
  linkColorScheme = 'posneg', 
  layout = c("nicely", "force", "circle"),
  variables = VariableMetadataList(), 
  ...
) {
  layout <- veupathUtils::matchArg(layout)

  # any additional validation and filtering are handled by the CorrelationLinkList constructor
  new("CorrelationNetwork", 
    links=CorrelationLinkList(object, linkColorScheme), 
    nodes=NodeList(object, layout), 
    linkColorScheme=linkColorScheme, 
    variableMapping=variables,
    correlationCoefThreshold=correlationCoefThreshold,
    pValueThreshold=pValueThreshold
  )
})

#' @rdname CorrelationNetwork
#' @aliases CorrelationNetwork,missing,missing,missing
setMethod("Network", signature("missing", "missing", "missing"), function(
  object, 
  links, 
  nodes,
  correlationCoefThreshold = NULL,
  pValueThreshold = 0.05,
  linkColorScheme = 'none', 
  variables = VariableMetadataList(), 
  ...
) {
  new("CorrelationNetwork")
})
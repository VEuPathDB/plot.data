# Methods for the Network class

#' @include methods-Nodes.R
# Fancy accessors
setGeneric("getNodes", function(object) standardGeneric("getNodes"))
setMethod("getNodes", "BaseNetwork", function(object) object@nodes)
setMethod("getNodeIds", "BaseNetwork", function(object) getNodeIds(object@nodes))
setGeneric("getLinks", function(object) standardGeneric("getLinks"))
setMethod("getLinks", "BaseNetwork", function(object) object@links)
setGeneric("getLinkColorScheme", function(object) standardGeneric("getLinkColorScheme"))
setMethod("getLinkColorScheme", "BaseNetwork", function(object) object@linkColorScheme)
# No setters! Once created, a network should only be updated via network methods

setMethod("getDegrees", "BaseNetwork", function(object) getDegrees(getNodes(object)))
setMethod("getCoords", "BaseNetwork", function(object) getCoords(getNodes(object)))

## General network methods

#' Get isolated nodes
#' 
#' Returns a list of nodes that have no links
#' @param net A Network object
#' @export
setGeneric("getIsolatedNodes", function(net) standardGeneric("getIsolatedNodes"))

#' @export
setMethod("getIsolatedNodes", "BaseNetwork", function(net) {
  nodes <- getNodes(net)
  links <- getLinks(net)

  nodeIdsWithLinks <- NodeIdList(union(getSourceNodes(links), getTargetNodes(links)))
  isolatedNodeIds <- setdiff(getNodeIds(nodes), getNodeIds(nodeIdsWithLinks))
  isolatedNodes <- NodeList(nodes[which(getNodeIds(nodes) %in% isolatedNodeIds)])

  return(isolatedNodes)
})


#' Remove isolated nodes
#' 
#' Removes nodes that have no links
#' @param net A Network object
#' @param verbose If TRUE, will print messages
#' @export
setGeneric("pruneIsolatedNodes", function(net, verbose = c(TRUE, FALSE)) standardGeneric("pruneIsolatedNodes"))

#' @export
setMethod("pruneIsolatedNodes", "BaseNetwork", function(net, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)
  nodes <- getNodes(net)
  isolatedNodeIds <- getNodeIds(getIsolatedNodes(net))

  if (length(isolatedNodeIds) > 0) {
    net@nodes <- nodes[which(!getNodeIds(nodes) %in% isolatedNodeIds)]
    validObject(net)
    veupathUtils::logWithTime(paste('Found and removed', length(isolatedNodeIds), 'isolated nodes.'), verbose)
  } else {
    veupathUtils::logWithTime('No isolated nodes found.', verbose)
  }

  validObject(net)
  return(net)
})

getLinkUniqueString <- function(link) {
  paste0(id(source(link)), ":||:", id(target(link)))
}

#' Find duplicate links
#' 
#' Returns a list of links that are redundant
#' @param net A Network object
#' @export
setGeneric("getDuplicateLinks", function(net) standardGeneric("getDuplicateLinks"))

#' @export
setMethod("getDuplicateLinks", "BaseNetwork", function(net) {
  links <- getLinks(net)

  # check for links that have the same source and target node as another link
  linkUniqueStrings <- sapply(links, getLinkUniqueString)
  dupLinks <- links[which(duplicated(linkUniqueStrings))]
  
  return(dupLinks)
})

#' Remove Duplicate links
#' 
#' Removes links that are redundant
#' @param net A Network object
#' @param verbose If TRUE, will print messages
#' @export
setGeneric("pruneDuplicateLinks", function(net, verbose = c(TRUE, FALSE)) standardGeneric("pruneDuplicateLinks"))

#' @export
setMethod("pruneDuplicateLinks", "BaseNetwork", function(net, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)
  links <- getLinks(net)

  dupLinks <- getDuplicateLinks(net)
  if (length(dupLinks) > 0) {
    linkUniqueStrings <- sapply(links, getLinkUniqueString)
    net@links <- links[which(!duplicated(linkUniqueStrings))]
    validObject(net)
    veupathUtils::logWithTime(paste('Found and removed', length(dupLinks), 'duplicate links.'), verbose)
  } else {
    veupathUtils::logWithTime('No duplicate links found.', verbose)
  }

  validObject(net)
  return(net)
})

#' Prune Links by Predicate
#' 
#' Removes links that satisfy a predicate
#' @param net A Network object
#' @param predicate A function that takes a link and returns a boolean
#' @param verbose If TRUE, will print messages
#' @param ... additional arguments passed to the predicate
#' @export
setGeneric("pruneLinksByPredicate", function(net, predicate, verbose = c(TRUE, FALSE), ...) standardGeneric("pruneLinksByPredicate"))

#' @export
setMethod("pruneLinksByPredicate", "BaseNetwork", function(net, predicate, verbose = c(TRUE, FALSE), ...) {
  verbose <- veupathUtils::matchArg(verbose)
  links <- getLinks(net)
  net@links <- links[which(!sapply(links, predicate, ...))]
  validObject(net)
  veupathUtils::logWithTime(paste('Found and removed', length(links) - length(net@links), 'links.'), verbose)
  validObject(net)
  return(net)
})

linkAboveWeightThreshold <- function(link, threshold) {
  return(weight(link) > threshold)
}

#' Prune Links by Weight
#' 
#' Removes links that have a weight above a threshold. This is a convenience
#' function that calls pruneLinksByPredicate.
#' @param net A Network object
#' @param threshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksAboveWeight <- function(net, threshold, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneLinksByPredicate(net = net, predicate = linkAboveWeightThreshold, threshold = threshold, verbose = verbose))
}

linkBelowWeightThreshold <- function(link, threshold) {
  return(weight(link) < threshold)
}


#' Prune Links by Weight
#' 
#' Removes links that have a weight below a threshold. This is a convenience
#' function that calls pruneLinksByPredicate.
#' @param net A Network object
#' @param threshold The threshold
#' @param verbose If TRUE, will print messages
#' @export
pruneLinksBelowWeight <- function(net, threshold, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  return(pruneLinksByPredicate(net = net, predicate = linkBelowWeightThreshold, threshold = threshold, verbose = verbose))
}

toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert Network object to JSON
#'
#' Converts a Network object to JSON
#' @param object A Network object
#' @param named boolean that declares if names should be included
setMethod(toJSONGeneric, "BaseNetwork", function(object, named = c(TRUE, FALSE)) {
  
  named <- veupathUtils::matchArg(named)    
  tmp <- character()

  nodes_json <- veupathUtils::toJSON(object@nodes, named = FALSE)
  links_json <- veupathUtils::toJSON(object@links, named = FALSE)

  tmp <- paste0('"nodes":', nodes_json, ',"links":', links_json)
  tmp <- paste0('"data":{', tmp, '}')
  tmp <- paste0('{', tmp, ',"config":{"variables":{', veupathUtils::toJSON(object@variableMapping, named = FALSE), '}}}')
  
  if (named) tmp <- paste0('{"network":', tmp, '}')

  return(tmp)
})

#' Write network json to tmp file
#' 
#' This function returns the name of a json file which it has
#' written an object out to.
#' @param x an object to convert to json and write to a tmp file
#' @param pattern optional tmp file prefix
#' @param verbose boolean that declares if logging is desired
#' @return character name of a tmp file w ext *.json
#' @export
#' @rdname writeNetworkJSON
setGeneric("writeNetworkJSON", function(x, pattern = NULL, verbose = c(TRUE, FALSE)) standardGeneric("writeNetworkJSON"), signature = c("x"))

#' @rdname writeNetworkJSON
#' @aliases writeNetworkJSON,Network-method
setMethod("writeNetworkJSON", "BaseNetwork", function(x, pattern=NULL, verbose = c(TRUE, FALSE)) {
  net <- x
  verbose <- veupathUtils::matchArg(verbose)

  outJson <- veupathUtils::toJSON(net, verbose)
  if (is.null(pattern)) { 
    pattern <- attr(net, 'class')[1]
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)
  veupathUtils::logWithTime(paste('New output file written:', outFileName), verbose)

  return(outFileName)
})
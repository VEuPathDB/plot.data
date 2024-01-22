# Methods for the Network class


# Fancy accessors
setGeneric("getNodes", function(object) standardGeneric("getNodes"))
setMethod("getNodes", "Network", function(object) object@nodes)
setGeneric("getLinks", function(object) standardGeneric("getLinks"))
setMethod("getLinks", "Network", function(object) object@links)
setGeneric("getLinkColorScheme", function(object) standardGeneric("getLinkColorScheme"))
setMethod("getLinkColorScheme", "Network", function(object) object@linkColorScheme)
# No setters! Once created, a network should only be updated via network methods


## General network methods

#' Get isolated nodes
#' 
#' Returns a list of nodes that have no links
#' @param net A Network object
#' @export
setGeneric("getIsolatedNodes", function(net) standardGeneric("getIsolatedNodes"))

#' @export
setMethod("getIsolatedNodes", "Network", function(net) {
  nodes <- getNodes(net)
  links <- getLinks(net)

  nodesWithLinks <- NodeList(union(getSourceNodes(links), getTargetNodes(links)))
  isolatedNodeIds <- setdiff(getNodeIds(nodes), getNodeIds(nodesWithLinks))
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
setMethod("pruneIsolatedNodes", "Network", function(net, verbose = c(TRUE, FALSE)) {
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
  paste0(id(source(link)), id(target(link)))
}

#' Find duplicate links
#' 
#' Returns a list of links that are redundant
#' @param net A Network object
#' @export
setGeneric("getDuplicateLinks", function(net) standardGeneric("getDuplicateLinks"))

#' @export
setMethod("getDuplicateLinks", "Network", function(net) {
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
setMethod("pruneDuplicateLinks", "Network", function(net, verbose = c(TRUE, FALSE)) {
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
setMethod("pruneLinksByPredicate", "Network", function(net, predicate, verbose = c(TRUE, FALSE), ...) {
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
pruneLinksByWeight <- function(net, threshold, verbose = c(TRUE, FALSE)) {
  return(pruneLinksByPredicate(net = net, predicate = linkAboveWeightThreshold, threshold = threshold, verbose = verbose))
}


## these look like things that should be made into github issues..
# Get Degree list
# Get Weighted Degree list
# etc.
# Threshold network by edge weight
# Assign color scheme


## this looks like it should be in a different pr..

# #' Write json to local tmp file
# #'
# #' This function returns the name of a json file which it has
# #' written a network object out to.
# #' @param net a data.table to convert to json and write to a tmp file
# #' @param pattern optional tmp file prefix
# #' @param verbose boolean that declares if logging is desired
# #' @return character name of a tmp file w ext *.json
# #' @importFrom jsonlite toJSON
# #' @export
# writeNetworkToJSON <- function(net, pattern=NULL, verbose = c(TRUE, FALSE) ) {
#   verbose <- veupathUtils::matchArg(verbose)

#   outJson <- getNetworkJSON(net, verbose)
#   if (is.null(pattern)) { 
#     pattern <- attr(net, 'class')[1]
#     if (is.null(pattern)) {
#       pattern <- 'file'
#     } 
#   }
#   outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
#   write(outJson, outFileName)
#   veupathUtils::logWithTime(paste('New output file written:', outFileName), verbose)

#   return(outFileName)
# }

# # Write a network to a json string
# getNetworkJSON <- function(net, verbose = c(TRUE, FALSE)) {

#   networkAttributes <- attributes(net)

#   # Covert all columns to character
#   netChar <- data.frame(lapply(net, as.character))

#   # Whenever a node is referenced, it should be in the form {id: nodeid}. Update this
#   # for both the list of nodes, and the source + target columns
#   nodeList <- data.frame(id = networkAttributes$nodes)
#   netChar$source <- lapply(netChar$source, function(node) { return(list(id=jsonlite::unbox(node)))})
#   netChar$target <- lapply(netChar$target, function(node) { return(list(id=jsonlite::unbox(node)))})

#   obj <- list(
#     nodes = nodeList,
#     links = netChar
#   )

#   # Add additional properties for other network classes
#   if ('column1NodeIDs' %in% names(networkAttributes)) obj$column1NodeIDs <- networkAttributes$column1NodeIDs
#   if ('column2NodeIDs' %in% names(networkAttributes)) obj$column2NodeIDs <- networkAttributes$column2NodeIDs


#   # Covert to json string
#   json <- jsonlite::toJSON(obj, na=NULL)


#   return(json)
# }


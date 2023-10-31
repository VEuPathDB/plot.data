# Network class
check_network <- function(object) {

  errors <- character()

  # Check that the linkColorScheme matches link weights

  return(if (length(errors) == 0) TRUE else errors)
}

#' Network
#' 
#' A class for representing networks. A network is composed of nodes and links (edges, connections, etc.). A link is represented
#' as a pair of nodes, with optional attributes such as weight. To represent a network, we need the list of links in the network, and we need
#' to know if there are any nodes that do not have any links (since that means will not show up in the list of links). Additionally,
#' the network can have other properties such as direction, multiple levels, colored nodes, and more (all coming soon!)
#' 
#' @slot links LinkList object
#' @slot nodes NodeList object defining the nodes in the network. Some nodes may not have any links.
#' @slot linkColorScheme string defining the type of coloring scheme the links follow. Options are 'none' (default) and 'posneg'.
#' Use a method assignLinkColors() to assign colors to links and set this slot's value.
#' 
#' @name Network-class
#' @rdname Network-class
#' @export
Network <- setClass("Network", 
  representation(
    links = "LinkList",
    nodes = "NodeList",
    linkColorScheme = "character"
  ), prototype = prototype(
    links = LinkList(),
    nodes = NodeList(),
    linkColorScheme = 'none'
  ),
  validity = check_network
)



# newNetwork <- function(df = data.frame(),
#                        sourceNodeColumn = character(),
#                        targetNodeColumn = character(),
#                        linkWeightColumn = NULL,
#                        nodeIDs = NULL,
#                        linkColorScheme = c('none', 'posneg'),
#                        verbose = logical(),
#                        class = character()
# ) {

#   # Input checks
#   linkColorScheme <- veupathUtils::matchArg(linkColorScheme)
#   if (!identical(linkColorScheme, 'none') && is.null(linkWeightColumn)) {
#     stop('A linkWeightColumn is required for converting link weights to a color scheme')
#   }

#   # Create a data table from df
#   dt <- data.table::as.data.table(df)

#   if (!is.null(nodeIDs)) {
#     if (any(duplicated(nodeIDs))) stop('nodeIDs must be unique')
#     if (!all(c(dt[[sourceNodeColumn]], dt[[targetNodeColumn]]) %in% nodeIDs)) {
#       stop('the nodeIDs argument must contain all nodes seen in dt')
#     }
#     nodeIDs <- sort(unique(nodeIDs))
#   } else {
#     nodeIDs <- sort(unique(c(dt[[sourceNodeColumn]], dt[[targetNodeColumn]])))
#   }

#   ## Create the network
#   # Subset the dt to the columns we care about
#   networkColumnNames <- c('source', 'target')
#   if (!is.null(linkWeightColumn)) networkColumnNames <- c(networkColumnNames, 'weight')
#   setnames(dt, c(sourceNodeColumn, targetNodeColumn, linkWeightColumn), networkColumnNames, skip_absent=TRUE)
#   dt <- dt[, ..networkColumnNames]

#   # Add link color if requested
#   if (identical(linkColorScheme, 'posneg')) {
#     dt[, color:=sign(as.numeric(weight))]
#   }

#   # Add attributes
#   attr <- attributes(dt)
#   attr$nodes <- nodeIDs
#   attr$class <- c(class, 'network', attr$class)
#   attr$linkColorScheme <- linkColorScheme

#   veupathUtils::setAttrFromList(dt, attr)
#   net <- validateNetwork(dt, verbose)
#   veupathUtils::logWithTime('Network object successfully created.', verbose)

#   return(net)
# }

# validateNetwork <- function(net, verbose) {

#   networkAttributes <- attributes(net)

#   # Check that all source and target nodes are in nodes
#   if (!all(c(net$source, net$target) %in% networkAttributes$nodes)) {
#     stop('Found nodes in the network that are not included in the nodes attribute')
#   }

#   class <- attr(net, 'class')
#   stopifnot(is.character(class))

#   veupathUtils::logWithTime("Network object validated.", verbose)

#   return(net)
# }


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

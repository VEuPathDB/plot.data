# Methods for the Network class


# Fancy accessors
setGeneric("getNodes", function(object) standardGeneric("getNodes"))
setMethod("getNodes", "Network", function(object) object@nodes)
setGeneric("getLinks", function(object) standardGeneric("getLinks"))
setMethod("getLinks", "Network", function(object) object@links)
setGeneric("getLinkColorScheme", function(object) standardGeneric("getLinkColorScheme"))
setMethod("getLinkColorScheme", "Network", function(object) object@linkColorScheme)
# No setters! Once created, a network should only be updated via network methods

# Remove isolated nodes
# Get isolated nodes
# Remove redundant links
# Remove redundant nodes
# Get Degree list
# Get Weighted Degree list
# etc.
# Threshold network by edge weight
# Assign color scheme



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


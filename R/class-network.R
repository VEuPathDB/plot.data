#' @param dt data table with one row per link. Columns represent source node, target node, edge weight, and other edge data
newNetwork <- function(dt = data.table(),
                       sourceNodeColumn = character(),
                       targetNodeColumn = character(),
                       linkWeightColumn = NULL,
                       nodeIDs = NULL,
                       linkColorScheme = c('none', 'posneg'),
                       nodeColorScheme = c('none', 'degree'),
                       directed = c(FALSE, TRUE),
                       verbose = logical(),
                       class = character()
) {

  linkColorScheme <- veupathUtils::matchArg(linkColorScheme)
  if (!identical(linkColorScheme, 'none') && is.null(linkWeightColumn)) {
    stop('A linkWeightColumn is required for converting link weights to a color scheme')
  }
  nodeColorScheme <- veupathUtils::matchArg(nodeColorScheme) # Placeholder: Not yet implemented
  directed <- veupathUtils::matchArg(directed) # Placeholder: Not yet implemented

  # Check for self edges (maybe uses boolean allowSelfLinks)
  # If nodeIDs, check to see if any have no edges. Also ensure all nodes in the dt are in nodeIDs


  # For now, all we need to do is to subset dt to columns that matter, then add the link color column
  networkColumnNames <- c('source', 'target')
  if (!is.null(linkWeightColumn)) networkColumnNames <- c(networkColumnNames, 'linkWeight')
  setnames(dt, c(sourceNodeColumn, targetNodeColumn, linkWeightColumn), networkColumnNames, skip_absent=TRUE)
  dt <- dt[, ..networkColumnNames]


  if (identical(linkColorScheme, 'posneg')) {
    dt[, linkColor:=sign(as.numeric(linkWeight))]
  }

  # So dt will be the links part of the response, and nodes wil be the nodes part. The bp net class 
  # just adds the attributes column1NodeIDs and column2NodeIDs

  attr <- attributes(dt)
  attr$nodes <- if(is.null(nodeIDs)) sort(unique(c(dt[['source']], dt[['target']]))) else sort(nodeIDs)
  attr$class <- c(class, 'network', attr$class)
  attr$linkColorScheme <- linkColorScheme

  veupathUtils::setAttrFromList(dt, attr)
  net <- validateNetwork(dt, verbose)
  veupathUtils::logWithTime('Network object successfully created.', verbose)

  return(net)
}

validateNetwork <- function(net, verbose) {

  # Could check that all source and target are still in nodeIDs

  class <- attr(net, 'class')
  stopifnot(is.character(class))

  veupathUtils::logWithTime("Network object validated.", verbose)

  return(net)
}


#' Write json to local tmp file
#'
#' This function returns the name of a json file which it has
#' written a data.table object out to.
#' @param .pd a data.table to convert to json and write to a tmp file
#' @param pattern optional tmp file prefix
#' @return character name of a tmp file w ext *.json
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite prettify
#' @export
writeNetworkToJSON <- function(net, pattern=NULL, verbose = c(TRUE, FALSE) ) {
  verbose <- veupathUtils::matchArg(verbose)

  outJson <- getNetworkJSON(net, verbose)
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
}

# Just write the json part
getNetworkJSON <- function(net, verbose = c(TRUE, FALSE)) {

  networkAttributes <- attributes(net)
  # Network json object should have nodes = Node[], links = Link[]

  # Covert all columns to character
  netChar <- data.frame(lapply(net, as.character))

  # Extract the list of node ids
  nodeList <- data.frame(id = networkAttributes$nodes)

  obj <- list(
    nodes = nodeList,
    links = netChar
  )

  # Optional additional props if exist are column1NodeIDs and 2
  if ('column1NodeIDs' %in% names(networkAttributes)) obj$column1NodeIDs <- networkAttributes$column1NodeIDs
  if ('column2NodeIDs' %in% names(networkAttributes)) obj$column2NodeIDs <- networkAttributes$column2NodeIDs


  # Covert to json string
  json <- jsonlite::toJSON(obj, na=NULL)


  return(json)
}

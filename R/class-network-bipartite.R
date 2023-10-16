newBipartiteNetwork <- function(df = data.frame(),
                                sourceNodeColumn = character(),
                                targetNodeColumn = character(),
                                linkWeightColumn = NULL,
                                nodeIDs = NULL,
                                linkColorScheme = c('none', 'posneg'),
                                nodeColorScheme = c('none', 'degree'),
                                verbose = logical()
) {

  # Create a data table from df
  dt <- data.table::as.data.table(df)

  # Assume the source nodes are column 1, and the target are all in column 2.
  column1NodeIDs <- sort(unique(dt[[sourceNodeColumn]]))
  column2NodeIDs <- sort(unique(dt[[targetNodeColumn]]))

  # TODO check that no nodes are in both cols

  # Create a basic network. We'll threshold the edges in here so we want to know about all the nodes 
  # before that happens. (add boolean removeIsolatedNodes and linkWeightThreshold fn i guess...)
  net <- newNetwork(dt = dt,
                    sourceNodeColumn = sourceNodeColumn,
                    targetNodeColumn = targetNodeColumn,
                    linkWeightColumn = linkWeightColumn,
                    nodeIDs = nodeIDs,
                    linkColorScheme = linkColorScheme,
                    nodeColorScheme = nodeColorScheme,
                    directed = FALSE,
                    verbose = verbose,
                    class = 'bipartite'
  )

  attr <- attributes(net)
  attr$column1NodeIDs <- column1NodeIDs
  attr$column2NodeIDs <- column2NodeIDs

  veupathUtils::setAttrFromList(net, attr)
  net <- validateNetwork(net, verbose)
  veupathUtils::logWithTime('Network object successfully created.', verbose)

  return(net)
}

validateBipartiteNetwork <- function(bpnet, verbose) {

  class <- attr(bpnet, 'class')
  stopifnot(is.character(class))

  veupathUtils::logWithTime("Bipartite network object validated.", verbose)

  return(net)
}

#' Create bipartite network
#'
#' This function returns the name of a json file which it has
#' written a data.table object out to.
#' @param .pd a data.table to convert to json and write to a tmp file
#' @param pattern optional tmp file prefix
#' @return bipartite network
#' @export
bipartiteNetwork <- function(df = data.frame(),
                            sourceNodeColumn = character(),
                            targetNodeColumn = character(),
                            linkWeightColumn = NULL,
                            nodeIDs = NULL,
                            linkColorScheme = c('none', 'posneg'),
                            nodeColorScheme = c('none', 'degree'),
                            verbose = logical()
) {

  bpnet <- newBipartiteNetwork(df = df,
                              sourceNodeColumn = sourceNodeColumn,
                              targetNodeColumn = targetNodeColumn,
                              linkWeightColumn = linkWeightColumn,
                              nodeIDs = nodeIDs,
                              linkColorScheme = linkColorScheme,
                              nodeColorScheme = nodeColorScheme,
                              verbose = verbose
  )

  return(bpnet)
  # outFile <- writeNetworkToJSON(bpnet, 'bipartiteNetwork', verbose=verbose) No, this just goes in the plugin

}




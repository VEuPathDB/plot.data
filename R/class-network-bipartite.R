newBipartiteNetwork <- function(df = data.frame(),
                                sourceNodeColumn = character(),
                                targetNodeColumn = character(),
                                linkWeightColumn = NULL,
                                nodeIDs = NULL,
                                linkColorScheme = c('none', 'posneg'),
                                verbose = logical()
) {

  # Assume the source nodes are column 1, and the target are column 2.
  column1NodeIDs <- sort(unique(df[[sourceNodeColumn]]))
  column2NodeIDs <- sort(unique(df[[targetNodeColumn]]))

  # Create a network
  net <- newNetwork(df = df,
                    sourceNodeColumn = sourceNodeColumn,
                    targetNodeColumn = targetNodeColumn,
                    linkWeightColumn = linkWeightColumn,
                    nodeIDs = nodeIDs,
                    linkColorScheme = linkColorScheme,
                    verbose = verbose,
                    class = 'bipartite'
  )

  # Add bipartite network attributes
  attr <- attributes(net)
  attr$column1NodeIDs <- column1NodeIDs
  attr$column2NodeIDs <- column2NodeIDs

  veupathUtils::setAttrFromList(net, attr)
  net <- validateNetwork(net, verbose)
  veupathUtils::logWithTime('Network object successfully created.', verbose)

  return(net)
}

validateBipartiteNetwork <- function(bpnet, verbose) {

  networkAttributes <- attributes(bpnet)
  
  # Ensure no node is in both columns
  if (any(networkAttributes$column1NodeIDs %in% networkAttributes$column2NodeIDs)) {
    stop('Nodes cannot reside in both columns of a bipartite network')
  }

  class <- attr(bpnet, 'class')
  stopifnot(is.character(class))

  veupathUtils::logWithTime("Bipartite network object validated.", verbose)

  return(net)
}



#' Create bipartite network
#'
#' This function creates an object that represents a bipartite network. A bipartite network is a network
#' in which there are two groups (columns) of nodes, and links only connect nodes in separate groups (columns).
#' @param df a data.frame of links, with one row per link. One column should contain the link source node, and one
#' column should contain the link target node. It will be assumed that all nodes in the source column are in one group, 
#' and that all nodes in the target column are in the second group.
#' @param sourceNodeColumn string defining the name of the column in df that corresponds to the source nodes
#' @param targetNodeColumn string defining the name of the column in df that corresponds to the target nodes
#' @param linkWeightColumn optional string defining the name of the column in df that corresponds to the weight of the link
#' @param nodeIDs optional string array. Should contain at least all nodes in the sourceNodeColumn and targetNodeColumn of df.
#' May also include nodes that do not have any links (isolated nodes). If not specified, the complete list of network nodes
#' will be the union of those in the sourceNodeColumn and targetNodeColumn.
#' @param linkColorScheme string denoting the type of coloring scheme to apply to edges. Options are 'none' (default) which
#' which does not attempt to assign a color to links. The 'posneg' option assigns the link color to the sign of the linkWeightColumn.
#' If linkColorSheme is not 'none', a linkWeightColumn must be specified.
#' @param verbose boolean to determine if time-stamped logging is desired.
#' @return bipartite network
#' @export
bipartiteNetwork <- function(df = data.frame(),
                            sourceNodeColumn = character(),
                            targetNodeColumn = character(),
                            linkWeightColumn = NULL,
                            nodeIDs = NULL,
                            linkColorScheme = c('none', 'posneg'),
                            verbose = logical()
) {

  bpnet <- newBipartiteNetwork(df = df,
                              sourceNodeColumn = sourceNodeColumn,
                              targetNodeColumn = targetNodeColumn,
                              linkWeightColumn = linkWeightColumn,
                              nodeIDs = nodeIDs,
                              linkColorScheme = linkColorScheme,
                              verbose = verbose
  )

  return(bpnet)

}




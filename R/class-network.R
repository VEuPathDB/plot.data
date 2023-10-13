#' @param dt data table with one row per link. Columns represent source node, target node, edge weight, and other edge data
newNetwork <- function(dt = data.table(),
                       sourceNodeColumn = character(),
                       targetNodeColumn = character(),
                       linkWeightColumn = NULL,
                       nodeIDs = NULL,
                       linkColorScheme = c('none', 'posneg'),
                       nodeColorScheme = c('none', 'degree'),
                       directed = c('FALSE', 'TRUE'),
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
  # .pd <- validatePlotdata(.dt)
  net <- dt
  veupathUtils::logWithTime('Network object created.', verbose)

  return(net)
}

validatePlotdata <- function(net) {
  .dt <- unclass(.pd)
  variables <- attr(.pd, 'variables')

  # also check there is only one collection in variables
  # collectionsCount <- sum(unlist(lapply(as.list(variables), function(x) {x@isCollection})))
  # if (collectionsCount > 1) stop("More than one collection variable was specified.")

  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

# # Additional accessor functions
# sampleSizeTable <- function(.pd) { attr(.pd, 'sampleSizeTable') }
# completeCasesTable <- function(.pd) { attr(.pd, 'completeCasesTable') }
# #these helpers need either validation or to be a dedicated method
# statsTable <- function(.pd) { attr(.pd, 'statsTable') }
# variablesList <- function(.pd) { as.list(attr(.pd, 'variables')) }

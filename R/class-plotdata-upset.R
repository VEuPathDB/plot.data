## What is the abstract thing that is an upset plot?
# Sets and intersections. Could be based on what upset.js does?
# An upset plot is a thing that has sets and intersections
# it has a type describing what these sets mean (mode - ex distinctIntersections)
# Does it include highlighting? (queries)
# .dt is the data table
# vars is a list of var names (for making VariableSpecs)
# mode tells us instersection | distinctIntersection | unions
# Assuming calculating missingness, not the completeness plot
# Goal is to get queries to also filter through this function
# no idea what 'queries' does yet. I'd like something that says type='upset' and it calculates missingness.
# In other situations we might need to calculate completeness
# Didn't implement union yet

newPoSet <- function(.dt = data.table::data.table(),
                     plotVariable = data.frame(), 
                     relation = character(),
                     queries = character(), 
                     ...,
                     class = character()) {

  attr <- attributes(.dt)


  # Need validation that all vars varIds are columns in .dt
  # Calculate power set of vars
  
  varNames <- plotVariable$variableId
  
  # Restrict the .dt to only those in varNames
  .dt <- .dt[, ..varNames]

  # Calculate the powerset of variables we care about. This line will get slowww with many vars
  pSet <- rje::powerSet(varNames)

  # Remove empty set
  pSet <- pSet[lapply(pSet, length) > 0]

  # Find intersections
  if (queries == 'none') {
    .sets <- lapply(pSet, getSetOverlap, data=.dt, relation=relation)  ## Needs to return a data table
  } else {
    stop("Unrecognized value for 'queries'.")
  }
  # else if queries filters the data or something do something else

  ### Note in the future we may want similar data returned for another plot type (Hesse diagram, pathways+enzymes, Sankey, etc.)
  ### The goal would be to either morph the upset class into a poset clas, or make a new plot that uses the getUpsetIntersections function.

  # Set to data table (for now)
  #### should be setDT
  .sets <- data.table::as.data.table(t(data.table::as.data.table(.sets)))

  data.table::setnames(.sets, c('sets', 'cardinality'))
  
  attr$names <- names(.sets) # Should we keep this?
  attr$mode <- relation
  attr$plotVariable <- plotVariable

  setAttrFromList(.sets, attr)
  
  return(.sets)
}



validatePoSet <- function(.sets) {

  # Check we have sets and names?
  return(.sets)
}

#' poSet Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per set. 
#' ANN UPDATE
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param relation String indicating type of set relations to return ('intersection', 'distinctIntersection', 'union')
#' @param queries Object that somehow describes how to filter data??
#' @return data.table plot-ready data
#' @export
poSet.dt <- function(data, 
                   map, 
                   relation = c('intersection','distinctIntersection'), 
                   queries = c('none')) {
  
  relation <- matchArg(relation)
  queries <- matchArg(queries)
  
  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }
  
  # Eventually should wrap this into something
  plotRef <- 'plotVariable'
  variableIds <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 2)
  entityIds <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 1)
  
  plotVariable <- data.frame('variableId' = unlist(variableIds), 'entityId' = unlist(entityIds))
  
  .poSet <- newPoSet(.dt = data,
                         plotVariable = plotVariable,
                         relation = relation,
                         queries = queries)
  
  .poSet <- validatePoSet(.poSet)
  
  return(.poSet)
}

#' Upset Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per set.
#' ANN UPDATE
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param relation String indicating type of set relations to return ('intersection', 'distinctIntersection', 'union')
#' @param queries Object that somehow describes how to filter data??
#' @return character name of json file containing plot-ready data
#' @export
poSet <- function(data, 
                   map, 
                   relation = c('intersection','distinctIntersection'), 
                   queries = c('none')) {
  
  relation <- matchArg(relation)
  queries <- matchArg(queries)
  
  .poSet <- poSet.dt(data, map, relation, queries)

  ## NEED A NEW WAY TO WRITE JSON!
  # outFileName <- writeJSON(.poSet, evilMode, 'barplot')
  
  # For now
  outList <- list(class = list('data'=.poSet, 'config'=attr(.poSet, 'plotVariable')))
  outJson <- jsonlite::toJSON(outList)
  write(outJson, 'poSetTest.json')
  
  return(outFileName)
}
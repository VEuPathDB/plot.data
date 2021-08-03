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
                     vars = data.frame(), 
                     relation = character(),
                     queries = character(), 
                     ...,
                     class = character()) {

  attr <- attributes(.dt)


  # Need validation that all vars varIds are columns in .dt
  # Calculate power set of vars
  
  varNames <- vars$variableId
  
  # Restrict the .dt to only those in varNames
  .dt <- .dt[, ..varNames]

  # Calculate the powerset of variables we care about. This line will get slowww with many vars
  pSet <- rje::powerSet(varNames)

  # Remove empty set
  pSet <- pSet[lapply(pSet, length) > 0]

  # Find intersections
  if (queries == 'missingness') {
    .sets <- lapply(pSet, getUpsetIntersections, data=.dt, mode=relation)  ## Needs to return a data table
  }
  # else if queries filters the data or something do something ele

  # Set to data table (for now)
  #### pls help
  .sets <- data.table::as.data.table(t(data.table::as.data.table(.sets)))

  data.table::setnames(.sets, c('sets', 'cardinality'))
  
  attr$names <- names(.sets) # Should we keep this?
  attr$mode <- relation
  attr$vars <- vars

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
#' plot-ready data with one row per set. Columns 
#' 'sets' and 'cardinality' contain the raw data for plotting.
#' There are three options for calculating set relation cardinality. \cr
#' 1) 'intersection' finds the number of elements  \cr
#' 2) 'count' occurrences of values from data.table input \cr 
#' 3) 'proportion' of occurrences of values from data.table input \cr 
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param relation String indicating type of set relations to return ('intersection', 'distinctIntersection', 'union')
#' @param queries Object that somehow describes how to filter data??
#' @return data.table plot-ready data
#' @export
poSet.dt <- function(data, 
                   map, 
                   relation = c('intersection','distinctIntersection'), 
                   queries = c('missingness')) {
  
  relation <- matchArg(relation)
  queries <- matchArg(queries)
  
  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }
  
  aVariable <- plotRefsMapToList(map, 'aVariable')
  
  plotRef <- 'aVariable'
  variableIds <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 2)
  entityIds <- lapply(map$id[map$plotRef == plotRef], strSplit, ".", 4, 1)
  
  vars <- data.frame('variableId' = unlist(variableIds), 'entityId' = unlist(entityIds))
  
  # xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  # if (is.null(xAxisVariable$variableId)) {
  #   stop("Must provide xAxisVariable for plot type bar.")
  # }
  
  .poSet <- newPoSet(.dt = data,
                         vars = vars,
                         relation = relation,
                         queries = queries)
  
  .poSet <- validatePoSet(.poSet)
  
  return(.poSet)
}

#' Upset Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'group' and 'panel' specify the group the series data belongs to.
#' There are three options to calculate y-values for plotting. \cr
#' 1) raw 'identity' of values from data.table input \cr
#' 2) 'count' occurrences of values from data.table input \cr 
#' 3) 'proportion' of occurrences of values from data.table input \cr
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @return character name of json file containing plot-ready data
#' @export
poSet <- function(data, 
                   map, 
                   relation = c('intersection','distinctIntersection'), 
                   queries = c('missingness')) {
  
  relation <- matchArg(relation)
  queries <- matchArg(queries)
  
  .poSet <- poSet.dt(data, map, relation, queries)

  ## NEED A NEW WAY TO WRITE JSON!
  # outFileName <- writeJSON(.poSet, evilMode, 'barplot')
  
  # For now
  outList <- list(class = list('data'=.poSet, 'config'=attr(.poSet, 'vars')))
  outList$mode <- attr(.poSet, 'mode')
  outJson <- jsonlite::toJSON(outList)
  write(outJson, 'poSetTest.json')
  
  return(outFileName)
}
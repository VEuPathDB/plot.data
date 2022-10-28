newHeatmapPD <- function(.dt = data.table::data.table(),
                         variables = new("VariableMetadataList"),
                         value = character(),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "heatmap")

  attr <- attributes(.pd)
  variables <- attr$variables

  #NOTE: one or the other of these could be a list for 'collection'
  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  #NOTE: this for the case of 'series'
  z <- veupathUtils::findColNamesFromPlotRef(variables, 'zAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  if (value == 'collection') {
    data <- groupSplit(data, x, y, NULL, NULL, panel)
  } else if (value == 'series' ) { 
    data <- data[order(data[[x]]),]
    data[[x]] <- as.factor(data[[x]])
    data[[y]] <- as.factor(data[[y]])
    data <- groupSplit(data, x, y, z, NULL, panel, longToWide = TRUE)
  } else {
    stop('Unrecognized argument to "value".')
  } 
  attr$names <- names(.pd)

  veupathUtils::setAttrFromList(.pd, attr)
  
  return(.pd)
}

validateHeatmapPD <- function(.heatmap) {
  variables <- attr(.heatmap, 'variables')
  if (!veupathUtils::findDataTypesFromPlotRef(variables, 'zAxis') %in% c('NUMBER')) {
    stop('The dependent axis must be of type number or date for heatmapplot.')
  }

  return(.heatmap)
}

#' Heatmap as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap. \cr
#' 1) 'collection' of numeric variables vs single categorical \cr
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils::VariableMetadataList
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @export
heatmap.dt <- function(data, variables, 
                       value = c('series', 'collection'), 
                       evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                       verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type scatter.")
  }
  yVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'yAxis')
  if (is.null(yVM)) {
    stop("Must provide y-axis variable for plot type scatter.")
  }
 
  .heatmap <- newHeatmapPD(.dt = data,
                            variables = variables,
                            value = value,
                            evilMode = evilMode,
                            verbose = verbose)

  .heatmap <- validateHeatmapPD(.heatmap, verbose)
  veupathUtils::logWithTime(paste('New heatmap object created with parameters value =', value, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.heatmap)

}

#' Heatmap data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Column 'table'
#'  contains a nested data.table of z-values for plotting. This 
#' table has a column for each x-axis entry and a row for each 
#' y-axis entry. Columns 'group' and 'panel' specify the group the 
#' series data belongs to. 
#' There are two ways to calculate z-values for the heatmap. \cr
#' 1) 'collection' of numeric variables vs single categorical \cr
#' 2) single numeric vs single categorical on a 'series' of dates
#' where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils VariableMetadataList
#' @param value String indicating which of the three methods to use to calculate z-values ('collection', 'series')
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @export
heatmap <- function(data, variables, 
                    value = c('series','collection'), 
                    evilMode = c('noVariables', 'allVariables', 'strataVariables')) {
  verbose <- veupathUtils::matchArg(verbose)
 
  .heatmap <- heatmap.dt(data, variables, value, evilMode, verbose)
  outFileName <- writeJSON(.heatmap, evilMode, 'heatmap', verbose)

  return(outFileName)
}

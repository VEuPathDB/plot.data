newBarPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),
                         value = character(),
                         barmode = character(),
                         overlayValues = veupathUtils::BinList(),
                         sampleSizes = logical(),
                         completeCases = logical(),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     overlayValues = overlayValues,
                     sampleSizes = sampleSizes,
                     completeCases = completeCases,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "barplot")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  .pd[[x]] <- as.character(.pd[[x]])

  if (value == 'identity') {
    .pd <- collapseByGroup(.pd, group, panel)
    veupathUtils::logWithTime('Value is set to `identity`. Resulting barplot object will represent raw values.', verbose)
  } else if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, x, 'dummy', group, panel, NULL, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))
    veupathUtils::logWithTime('Value is set to `count`. Resulting barplot object will represent counts of unique x-axis values per group.', verbose)
  } else if (value == 'proportion') {
    .pd$dummy <- 1
    .pd <- groupProportion(.pd, x, 'dummy', group, panel, NULL, barmode, collapse = T)
    data.table::setnames(.pd, c(group, panel, 'label', 'value'))
    veupathUtils::logWithTime('Value is set to `proportion`. If barmode is `group` the resulting barplot object will represent the relative proportions of unique x-axis values across groups. If barmode is `stack` the resulting barplot object will represent the proportions of unique x-axis values relative to the total x-axis values in that panel.', verbose)
  }
  
  attr$names <- names(.pd)
  
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateBarPD <- function(.bar, verbose) {
  veupathUtils::logWithTime('Barplot request has been validated!', verbose)

  return(.bar)
}

#' Bar Plot as data.table
#'
#' This function returns a data.table of 
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
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @return data.table plot-ready data
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils VariableMetadataList
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables')
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- veupathUtils::VariableMetadataList(
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'xvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'xAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'overlay', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'overlay'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns a data table with plot-ready data
#' dt <- bar.dt(df,map,value='count')
#' @export
bar.dt <- function(data, 
                   variables = variables, 
                   value = c('count', 'identity', 'proportion'), 
                   barmode = c('group', 'stack'), 
                   overlayValues = NULL,
                   sampleSizes = c(TRUE, FALSE),
                   completeCases = c(TRUE, FALSE),
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  barmode <- veupathUtils::matchArg(barmode)
  sampleSizes <- veupathUtils::matchArg(sampleSizes)
  completeCases <- veupathUtils::matchArg(completeCases)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide xAxisVariable for plot type bar.")
  }

  .bar <- newBarPD(.dt = data,
                    variables = variables,
                    value = value,
                    barmode = barmode,
                    overlayValues = overlayValues,
                    sampleSizes = sampleSizes,
                    completeCases = completeCases,
                    evilMode = evilMode,
                    verbose = verbose)

  .bar <- validateBarPD(.bar, verbose)
  veupathUtils::logWithTime(paste('New barplot object created with parameters value =', value,
                                                                              ', barmode =', barmode,
                                                                              ', sampleSizes = ', sampleSizes,
                                                                              ', completeCases = ', completeCases,
                                                                              ', evilMode =', evilMode,
                                                                              ', verbose =', verbose), verbose)

  return(.bar)
}

#' Bar Plot data file
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
#' - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
#' - when `allVariables` it will return 'no data' as a regular value for all variables. \cr
#' - when `noVariables` it will do the sensible thing and return complete cases only. \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils VariableMetadataList
#' @param value String indicating how to calculate y-values ('identity', 'count', 'proportion')
#' @param barmode String indicating if bars should be grouped or stacked ('group', 'stack')
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
#' variables <- veupathUtils::VariableMetadataList(
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'xvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'xAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'overlay', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'overlay'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns the name of a json file
#' bar(df,map,value='count')
#' @return character name of json file containing plot-ready data
#' @export
bar <- function(data, 
                variables = variables, 
                value = c('count', 'identity', 'proportion'), 
                barmode = c('group', 'stack'), 
                overlayValues = NULL,
                sampleSizes = c(TRUE, FALSE),
                completeCases = c(TRUE, FALSE),
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .bar <- bar.dt(data = data,
                 variables = variables,
                 value = value,
                 barmode = barmode,
		 overlayValues = overlayValues,
                 sampleSizes = sapleSizes,
                 completeCases = completeCases,
                 evilMode = evilMode,
                 verbose = verbose)

  outFileName <- writeJSON(.bar, evilMode, 'barplot', verbose)

  return(outFileName)
}

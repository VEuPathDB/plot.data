newMosaicPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),
                         statistic = character(),
                         columnReferenceValue = character(),
                         rowReferenceValue = character(),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "mosaic")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'),
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  isEvil <- ifelse(evilMode %in% c('allVariables', 'strataVariables'), TRUE, FALSE)
  
  if (!isEvil) {
    if (statistic == 'all') {
      # currently only valid for 2x2
      attr$allStatsTable <- panelAllStats(.pd, x, y, panel, columnReferenceValue, rowReferenceValue)
      veupathUtils::logWithTime('Calculated all relevant statistics.', verbose)
    } else if (statistic == 'chiSq') {
      attr$statsTable <- panelChiSq(.pd, x, y, panel)
      veupathUtils::logWithTime('Calculated chi-squared statistic.', verbose)
    }
  } else {
    veupathUtils::logWithTime('No statistics calculated when evilMode is `allVariables` or `strataVariables`.', verbose)
  }
  
  .pd <- panelTable(.pd, x, y, panel)

  attr$names <- names(.pd)  
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateMosaicPD <- function(.mosaic, verbose) {
  veupathUtils::logWithTime('Mosaic plot request has been validated!', verbose)

  return(.mosaic)
}


#' Mosaic plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to.
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
#' @param variables veupathUtil::VariableMetadataList
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'all', the second of which will return odds ratios and relative risk.
#' @param columnReferenceValue String representing a value present in the column names of the contingency table
#' @param rowReferenceValue String representing a value present in the row names of the contingency table
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = sample(c('1','2','3'), 100, replace=T), stringsAsFactors = F)
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
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns a data table with plot-ready data
#' dt <- mosaic.dt(df, map)
#' @export
mosaic.dt <- function(data, variables, 
                      statistic = NULL, 
                      columnReferenceValue = NA_character_,
                      rowReferenceValue = NA_character_,
                      evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                      verbose = c(TRUE, FALSE)) {

  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  isEvil <- ifelse(evilMode %in% c('allVariables', 'strataVariables'), TRUE, FALSE)
  if (isEvil && length(statistic)) {
    warning('evilModes `allVariables` and `strataVariables` are not compatible with statistics! Requested statistic will be ignored!')
  }

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type mosaic.")
  }
  yVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'yAxis')
  if (is.null(yVM)) {
    stop("Must provide y-axis variable for plot type mosaic.")
  }

  x <- veupathUtils::getColName(xVM@variableSpec)
  y <- veupathUtils::getColName(yVM@variableSpec)

  if (!is.null(statistic)) {
    if (!statistic %in% c('chiSq','all')) {
      stop('`statistic` argument must be one of either \'chiSq\' or \'all\', the second of which returns both odds ratios and relative risk.')
    }
    #na.rm should be safe, since x and y axes will later have NA removed anyhow in the plot.data parent class
    if ((data.table::uniqueN(data[[x]], na.rm = TRUE) > 2 || data.table::uniqueN(data[[y]], na.rm = TRUE) > 2) && statistic == 'bothRatios') {
      stop('Odds ratio and relative risk can only be calculated for 2x2 contingency tables. Please use statistic `chiSq` instead.')
    }
  } else {
    if (data.table::uniqueN(data[[x]], na.rm = TRUE) > 2 || data.table::uniqueN(data[[y]], na.rm = TRUE) > 2) {
      statistic <- 'chiSq'
    } else {
      statistic <- 'bothRatios'
    }
    veupathUtils::logWithTime(paste('No statistic specified, using:', ifelse(statistic=='chiSq', 'chi-squared', 'odds ratio and relative risk')), verbose)
  }
  
  .mosaic <- newMosaicPD(.dt = data,
                            variables = variables,
                            statistic = statistic,
                            columnReferenceValue = columnReferenceValue,
                            rowReferenceValue = rowReferenceValue,
                            evilMode = evilMode,
                            verbose = verbose)

  .mosaic <- validateMosaicPD(.mosaic, verbose)
  veupathUtils::logWithTime(paste('New mosaic plot object created with parameters statistic =', statistic, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.mosaic)
}

#' Mosaic data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'x' and 'y' contain the raw data for plotting. Column 'panel' 
#' specifies the panel the data belongs to. 
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
#' @param statistic String indicating which statistic to calculate. Vaid options are 'chiSq' and 'all', the second of which will return odds ratios and relative risk.
#' @param columnReferenceValue String representing a value present in the column names of the contingency table
#' @param rowReferenceValue String representing a value present in the row names of the contingency table
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = sample(c('1','2','3'), 100, replace=T), stringsAsFactors = F)
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
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns the name of a json file
#' mosaic(df, map)
#' @export
mosaic <- function(data, variables, 
                   statistic = NULL,
                   columnReferenceValue = NA_character_,
                   rowReferenceValue = NA_character_,
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  .mosaic <- mosaic.dt(data, variables, statistic, columnReferenceValue, rowReferenceValue, evilMode, verbose)
  outFileName <- writeJSON(.mosaic, evilMode, 'mosaic', verbose)

  return(outFileName)
}

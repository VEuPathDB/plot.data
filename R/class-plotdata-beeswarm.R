newBeeswarmPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),
                         jitter = NULL,
                         median = logical(),
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
                     inferredVarAxis = 'y',
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "beeswarm")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  xType <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  yType <- veupathUtils::findDataTypesFromPlotRef(variables, 'yAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  # Organize raw data and compute jittered values. Keeping all jittered values centered at 0 so that the front end can reorder if necessary.
  byCols <- colnames(.pd)[colnames(.pd) %in% c(x, group, panel)]
  rawWithJitter <- .pd[, list(rawData=lapply(.SD, as.vector),
                        jitteredValues=lapply(.SD, function(x, jitter) runif(length(x), min=(-jitter), max=jitter), jitter=jitter)), keyby=byCols]
  
  byColValues <- unique(.pd[, byCols, with=FALSE])
  rawWithJitter <- merge(rawWithJitter, byColValues, by=byCols, all=TRUE)

  rawWithJitter <- collapseByGroup(rawWithJitter, group, panel)
  # indexCols <- c(panel, group)
  # setkeyv(rawWithJitter, indexCols)
  
  .pd.base <- rawWithJitter
  logWithTime('Returning all points for beeswarm.', verbose)

  if (median) {
    median <- groupMedian(.pd, x, y, group, panel)
    median[[x]] <- NULL
    if (!is.null(key(median))) {
      .pd.base <- merge(.pd.base, median)
    } else {
      .pd.base <- cbind(.pd.base, median)
    }
    logWithTime('Calculated medians for beeswarm.', verbose)
  }
  
  .pd <- .pd.base
  data.table::setnames(.pd, x, 'label')
  attr$names <- names(.pd)
  setAttrFromList(.pd, attr)

  return(.pd)
}

validateBeeswarmPD <- function(.beeswarm, verbose) {
  variables <- attr(.beeswarm, 'variables')
  if (!veupathUtils::findDataTypesFromPlotRef(variables, 'yAxis') %in% c('NUMBER', 'INTEGER')) {
    stop('The dependent axis must be of type number for beeswarm.')
  }
  logWithTime('Beeswarm request has been validated!', verbose)

  return(.beeswarm)
}


#' Beeswarm Plot as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'jitteredValues' represent the x axis tick label and a random offset (one per y value), repsectively.
#' The 'rawData' column lists the y values to be plotted above each x axis tick. Columns 'group' and 'panel' specify
#' the group the data belong to. 
#' Optionally, can return median values per group
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
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = rnorm(100),
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
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'NUMBER'),
#'     dataShape = veupathUtils::DataShape(value = 'CONTINUOUS')
#'   )
#' )
#' 
#' # Returns a data table with plot-ready data
#' dt <- beeswarm.dt(df, map, jitter=0.3)
#' @export
beeswarm.dt <- function(data, variables,
                   jitter = NULL, 
                   median = c(FALSE, TRUE), 
                   overlayValues = NULL,
                   sampleSizes = c(TRUE, FALSE),
                   completeCases = c(TRUE, FALSE),
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {

  median <- matchArg(median)
  sampleSizes <- matchArg(sampleSizes)
  completeCases <- matchArg(completeCases)
  evilMode <- matchArg(evilMode)
  verbose <- matchArg(verbose)

  # Set default jitter to 0.1 (should also test is numeric)
  if (is.null(jitter)) {
    jitter <- 0.1
  } else if (!is.numeric(jitter)) {
    stop('jitter must be numeric for beeswarm plots.')
  }

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type beeswarm.")
  }
  
  yVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'yAxis')
  collectionVM <- veupathUtils::findCollectionVariableMetadata(variables)
  if (is.null(yVM) & is.null(collectionVM)) {
    stop("Must provide y-axis variable for plot type beeswarm.")
  }

  # Handle collectionVars
  if (!is.null(collectionVM)) {
    if (!collectionVM@plotReference@value %in% c('xAxis', 'facet1', 'facet2')) stop('Collection variable PlotReference must be either xAxis, facet1 or facet2 for beeswarm.')
  }

  .beeswarm <- newBeeswarmPD(.dt = data,
                    variables = variables,
                    jitter = jitter,
                    median = median,
                    overlayValues = overlayValues,
                    sampleSizes = sampleSizes,
                    completeCases = completeCases,
                    evilMode = evilMode,
                    verbose = verbose)

  .beeswarm <- validateBeeswarmPD(.beeswarm, verbose)
  logWithTime(paste('New beeswarm object created with parameters jitter=', jitter,
                                                                ', median =', median,
                                                                ', sampleSizes = ', sampleSizes,
                                                                ', completeCases = ', completeCases,
                                                                ', evilMode =', evilMode,
                                                                ', verbose =', verbose), verbose)

  return(.beeswarm) 

}

#' Beeswarm Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'label' and 'jitteredValues' represent the x axis tick label and a random offset (one per y value), repsectively.
#' The 'rawData' column lists the y values to be plotted above each x axis tick. Columns 'group' and 'panel' specify
#' the group the data belong to. 
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
#' @param jitter numeric indicating the maximum width by which to randomly offset points.
#' @param median boolean indicating whether to return median value per group (per panel)
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables')
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.yvar' = rnorm(100),
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
#'   ),
#'   veupathUtils::VariableMetadata(
#'     variableClass = veupathUtils::VariableClass(value = 'native'),
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'yAxis'),
#'     dataType = veupathUtils::DataType(value = 'NUMBER'),
#'     dataShape = veupathUtils::DataShape(value = 'CONTINUOUS')
#'   )
#' )
#' 
#' # Returns the name of a json file
#' beeswarm(df,map,jitter=0.3)
#' @export
beeswarm <- function(data, variables, 
                jitter = NULL, 
                median = c(FALSE, TRUE), 
                overlayValues = NULL,
                sampleSizes = c(TRUE, FALSE),
                completeCases = c(TRUE, FALSE),
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- matchArg(verbose)

  .beeswarm <- beeswarm.dt(data,
                 variables = variables,
                 jitter = jitter,
                 median = median,
                 overlayValues = overlayValues,
                 sampleSizes = sampleSizes,
                 completeCases = completeCases,
                 evilMode = evilMode,
                 verbose = verbose)

  outFileName <- writeJSON(.beeswarm, evilMode, 'beeswarm', verbose)

  return(outFileName)
}

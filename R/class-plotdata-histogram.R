#' @importFrom zoo as.yearmon
newHistogramPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),
                         viewport = list('xMin' = NULL,
                                         'xMax' = NULL),
                         binWidth,
                         binReportValue = character(),
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
                     inferredVarAxis = 'x',
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "histogram")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  xType <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  panel <- findPanelColName(veupathUtils::findVariableSpecFromPlotRef(variables, 'facet1'), 
                            veupathUtils::findVariableSpecFromPlotRef(variables, 'facet2'))

  if (!length(.pd[[x]])) {
    binSlider <- list('min'=jsonlite::unbox(NA), 'max'=jsonlite::unbox(NA), 'step'=jsonlite::unbox(NA))
    binSpec <- list('type'=jsonlite::unbox(binReportValue), 'value'=jsonlite::unbox(NA))
    viewport <- list('xMin'=0, 'xMax'=-Inf) 
    summary <- list('min'=jsonlite::unbox(""),
                    'q1'=jsonlite::unbox(""),
                    'median'=jsonlite::unbox(""),
                    'mean'=jsonlite::unbox(""),
                    'q3'=jsonlite::unbox(""),
                    'max'=jsonlite::unbox(""))
    attr$summary <- summary
    attr$viewport <- list('xMin'=jsonlite::unbox(""), 'xMax'=jsonlite::unbox(""))
    veupathUtils::logWithTime('No complete cases found.', verbose)
  } else {
    summary <- as.list(summary(.pd[[x]]))
    names(summary) <- c('min', 'q1', 'median', 'mean', 'q3', 'max')
    summary <- lapply(summary, as.character)
    summary <- lapply(summary, jsonlite::unbox)
    attr$summary <- summary
    veupathUtils::logWithTime('Supporting summary statistics calculated for histogram.', verbose)
  
    if (is.null(viewport)) {
      viewport <- findViewport(.pd[[x]], xType)
      veupathUtils::logWithTime('Determined default viewport.', verbose)
    } else {
      viewport <- validateViewport(viewport, xType, verbose)
    }
    attr$viewport <- lapply(viewport, as.character)
    attr$viewport <- lapply(attr$viewport, jsonlite::unbox)
    xVP <- adjustToViewport(.pd[[x]], viewport)

    if (binReportValue == 'binWidth') {
      if (is.null(binWidth)) {
        binWidth <- findBinWidth(xVP)
        veupathUtils::logWithTime('Determined ideal bin width.', verbose)
      }
      
      if (xType %in% c('NUMBER', 'INTEGER')) {
        binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(binWidth))
      } else {
        numericBinWidth <- as.numeric(gsub("[^0-9.-]", "", binWidth))
        if (is.na(numericBinWidth)) { numericBinWidth <- 1 }
        unit <- veupathUtils::trim(gsub("^[[:digit:]].", "", binWidth))
        binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(numericBinWidth), 'units'=jsonlite::unbox(unit))
      }
    } else {
      if (is.null(binWidth)) {
        numBins <- findNumBins(xVP)
        veupathUtils::logWithTime('Determined ideal number of bins.', verbose)
      } else {
        numBins <- binWidthToNumBins(xVP, binWidth)
        veupathUtils::logWithTime('Converted provided bin width to number of bins.', verbose)
      }
      binSpec <- list('type'=jsonlite::unbox('numBins'), 'value'=jsonlite::unbox(numBins))
    }
    binSlider <- findBinSliderValues(xVP, xType, binWidth, binReportValue)
    veupathUtils::logWithTime('Determined bin width slider min, max and step values.', verbose)
  }
  
  attr$binSpec <- binSpec
  attr$binSlider <- binSlider 

  if (value == 'count') {
    .pd <- binSize(.pd, x, group, panel, NULL, binWidth, viewport)
    veupathUtils::logWithTime('Value is set to `count`. Resulting histogram object will represent counts of unique x-axis bins per group.', verbose)
  } else if (value == 'proportion' ) {
    .pd <- binProportion(.pd, x, group, panel, NULL, binWidth, barmode, viewport)
    veupathUtils::logWithTime('Value is set to `proportion`. If barmode is `group` the resulting histogram object will represent the relative proportions of unique x-axis bins across groups. If barmode is `stack` the resulting histogram object will represent the proportions of unique x-axis bins relative to the total x-axis bins in that panel.', verbose)
  } else {
    stop('Unrecognized argument to "value".')
  }
  
  attr$names <- names(.pd)

  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

binSlider <- function(.histo) { attr(.histo, 'binSlider') }
binSpec <- function(.histo) { attr(.histo, 'binSpec') }
viewport <- function(.histo) { attr(.histo, 'viewport') }
binWidth <- function(.histo) { ifelse(attr(.histo, 'binSpec')$type == 'binWidth', attr(.histo, 'binSpec')$value, NULL) }
numBins <- function(.histo) { ifelse(attr(.histo, 'binSpec')$type == 'numBins', attr(.histo, 'binSpec')$value, NULL) }

validateBinSlider <- function(binSlider) {
  if (!is.list(binSlider)) {
    return(FALSE)
  } else{
    if (!all(c('max', 'min', 'step') %in% names(binSlider))) {
      return(FALSE)
    }
  }

  return(TRUE)
}

# possibly make viewport a class when we refactor for s4..
validateViewport <- function(viewport, xType, verbose) {
  if (!is.list(viewport)) {
    return(FALSE)
  } else{
    if (!all(c('xMax', 'xMin') %in% names(viewport))) {
      return(FALSE)
    }
  }

  if (xType %in% c('NUMBER', 'INTEGER')) {
    viewport$xMin <- as.numeric(viewport$xMin)
    viewport$xMax <- as.numeric(viewport$xMax)
  } else if (xType == 'DATE') {
    viewport$xMin <- as.Date(viewport$xMin, format='%Y-%m-%d')
    viewport$xMax <- as.Date(viewport$xMax, format='%Y-%m-%d')
  }
  veupathUtils::logWithTime('Provided viewport validated.', verbose)

  return(viewport)
}

validateHistogramPD <- function(.histo, verbose) {
  binSlider <- attr(.histo, 'binSlider')
  stopifnot(validateBinSlider(binSlider))
  variables <- attr(.histo, 'variables')
  xtype <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  xShape <- veupathUtils::findDataShapesFromPlotRef(variables, 'xAxis')
  if (!xShape == 'CONTINUOUS') {
    stop('The independent axis must be continuous for a histogram.')
  }
  binWidth <- attr(.histo, 'binWidth')
  if (!is.null(binWidth)) {
    if (xType == 'DATE' && !is.character(binWidth)) {
      stop("binWidth must be a character string for histograms of date values.")
    } else if (xType %in% c('NUMBER', 'INTEGER') && !is.numeric(binWidth)) {
      stop("binWidth must be numeric for histograms of numeric values.")
    }
  }
  veupathUtils::logWithTime('Histogram request has been validated!', verbose)
  
  return(.histo)
}

#' Histogram as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. It is possible to plot missingness in the stratification variables as an explicit 'No data' value using `evilMode`.
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
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param barmode String indicating if bars should be stacked or overlaid ('stack', 'overlay')
#' @param viewport List of min and max values to consider as the range of data
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @importFrom stringi stri_count_regex
#' @importFrom jsonlite unbox
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
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
#' viewport <- list('xMin'=min(df$xvar), 'xMax'=max(df$xvar))
#' 
#' # Returns a data table with plot-ready data
#' dt <- histogram.dt(df, map, binWidth=0.3, value='count', barmode='stack', viewport=viewport)
#' @export
histogram.dt <- function(data, 
                         variables, 
                         binWidth = NULL, 
                         value = c('count', 'proportion'), 
                         binReportValue = c('binWidth', 'numBins'),
                         barmode = c('stack', 'overlay'),
                         viewport = NULL,
                         overlayValues = NULL,
                         sampleSizes = c(TRUE, FALSE),
                         completeCases = c(TRUE, FALSE),
                         evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                         verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  barmode <- veupathUtils::matchArg(barmode)
  binReportValue <- veupathUtils::matchArg(binReportValue)
  sampleSizes <- veupathUtils::matchArg(sampleSizes)
  completeCases <- veupathUtils::matchArg(completeCases)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  if (is.null(xVM)) {
    stop("Must provide x-axis variable for plot type histogram.")
  } else {
    if (xVM@dataType@value %in% c('NUMBER', 'INTEGER') & !is.null(binWidth)) {
      binWidth <- suppressWarnings(as.numeric(binWidth))
      if (is.na(binWidth)) {
        stop("binWidth must be numeric for histograms of numeric values.")
      }
    }
  }

  .histo <- newHistogramPD(.dt = data,
                           variables = variables,
                           viewport = viewport,
                           binWidth = binWidth,
                           binReportValue = binReportValue,
                           value = value,
                           barmode = barmode,
                           overlayValues = overlayValues,
                           sampleSizes = sampleSizes,
                           completeCases = completeCases,
                           evilMode = evilMode,
                           verbose = verbose)

  .histo <- validateHistogramPD(.histo, verbose)
  veupathUtils::logWithTime(paste('New histogram object created with parameters viewport min =', viewport$xMin,
                                                                             ', viewport max =', viewport$xMax,
                                                                             ', binWidth =', binWidth,
                                                                             ', binReportValue =', binReportValue,
                                                                             ', value =', value,
                                                                             ', barmode =', barmode,
                                                                             ', sampleSizes = ', sampleSizes,
                                                                             ', completeCases = ', completeCases,
                                                                             ', evilMode =', evilMode,
                                                                             ', verbose =', verbose), verbose)

  return(.histo)
}

#' Histogram data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'x' and 'y' contain the bin label and count respectively. 
#' Column 'group' and 'panel' specify the group the series data 
#' belongs to. 
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
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date 
#' @param value String indicating how to calculate y-values ('count, 'proportion')
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param barmode String indicating if bars should be stacked or overlaid ('stack', 'overlay')
#' @param viewport List of min and max values to consider as the range of data
#' @param overlayValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @importFrom jsonlite unbox
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = rnorm(100),
#'                  'entity.overlay' = sample(c('red','green','blue'), 100, replace=T), stringsAsFactors = F)
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
#' viewport <- list('xMin'=min(df$xvar), 'xMax'=max(df$xvar))
#' 
#' # Returns the name of a json file
#' histogram(df, map, binWidth=0.3, value='count', barmode='stack', viewport=viewport)
#' @export
histogram <- function(data, 
                      variables, 
                      binWidth = NULL, 
                      value = c('count', 'proportion'), 
                      binReportValue = c('binWidth', 'numBins'), 
                      barmode = c('stack', 'overlay'),
                      viewport = NULL,
                      overlayValues = NULL,
                      sampleSizes = c(TRUE, FALSE),
                      completeCases = c(TRUE, FALSE),
                      evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                      verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .histo <- histogram.dt(data = data, 
                          variables = variables,
                          binWidth = binWidth,
                          value = value,
                          binReportValue = binReportValue,
                          barmode = barmode,
                          viewport = viewport,
			                    overlayValues = overlayValues,
                          sampleSizes = sampleSizes,
                          completeCases = completeCases,
                          evilMode = evilMode,
                          verbose = verbose)

  outFileName <- writeJSON(.histo, evilMode, 'histogram', verbose)

  return(outFileName)
}

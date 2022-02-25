newLinePD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         yAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         overlayVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable1 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         facetVariable2 = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         viewport = list('xMin' = NULL,
                                         'xMax' = NULL),
                         binWidth,
                         value = character(),
                         errorBars = logical(),
                         evilMode = logical(),
                         numeratorValues = character(),
                         denominatorValues = character(),
                         collectionVariableDetails = list('inferredVariable' = NULL,
                                               'inferredVarPlotRef' = NULL,
                                               'collectionVariablePlotRef' = NULL),
                         computedVariableMetadata = list('displayName' = NULL,
                                                         'displayRangeMin' = NULL,
                                                         'displayRangeMax' = NULL,
                                                         'collectionVariable' = NULL),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     yAxisVariable = yAxisVariable,
                     overlayVariable = overlayVariable,
                     facetVariable1 = facetVariable1,
                     facetVariable2 = facetVariable2,
                     evilMode = evilMode,
                     collectionVariableDetails = collectionVariableDetails,
                     computedVariableMetadata = computedVariableMetadata,
                     verbose = verbose,
                     class = "lineplot")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  xType <- attr$xAxisVariable$dataType
  y <- veupathUtils::toColNameOrNull(attr$yAxisVariable)
  yType <- attr$yAxisVariable$dataType
  group <- veupathUtils::toColNameOrNull(attr$overlayVariable)
  panel <- findPanelColName(attr$facetVariable1, attr$facetVariable2)

  if (yType == 'STRING') {
    if (is.null(numeratorValues)) {
      stop("Numerator values must be specified for categorical y-axes.")
    }
    if (is.null(denominatorValues)) {
      denominatorValues <- unique(.pd[[y]])
    }
    #validate num and denom values actually exist as part of the y values
    validateValues(numeratorValues, .pd[[y]])
    validateValues(denominatorValues, .pd[[y]])
    veupathUtils::logWithTime('Numerator and denominator values have been validated.', verbose)

    if (value != 'proportion') { stop('`value` parameter must be `proportion` for categorical y-axes.') }
  } else {
    if (!!length(c(numeratorValues,denominatorValues))) {
      warning("Numerator and/ or denominator values supplied for non-categorical y-axis. These will be ignored.")
    }

    if (value %ni% c('mean', 'median')) { stop('`value` parameter must be `mean` or `median` for numeric or date y-axes.')}
  } 

  # if no binWidth is provided, find one. if the user doesnt want binning they can set binWidth to 0
  # if someone complains about that well add a boolean param to indicate if binning is desired
  if (xType != 'STRING') {
    if (!length(.pd[[x]])) {
      binWidth <- 0
      binSlider <- list('min'=jsonlite::unbox(NA), 'max'=jsonlite::unbox(NA), 'step'=jsonlite::unbox(NA))
      binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(NA))
      viewport <- list('xMin'=0, 'xMax'=-Inf)
      attr$viewport <- list('xMin'=jsonlite::unbox(""), 'xMax'=jsonlite::unbox(""))
      veupathUtils::logWithTime('No complete cases found.', verbose)
    } else {  
      # think we need to take viewport as input, even if we dont want semantic zoom
      # for consistent bins across the annotated range, we need a consistent range/ bin start
      if (is.null(viewport)) {
        viewport <- findViewport(.pd[[x]], xType)
        veupathUtils::logWithTime('Determined default viewport.', verbose)
      } else {
        viewport <- validateViewport(viewport, xType, verbose)
      }
      attr$viewport <- lapply(viewport, as.character)
      attr$viewport <- lapply(attr$viewport, jsonlite::unbox)
  
      if (is.null(binWidth)) {
        # if we want semantic zoom, then use xVP here instead, see histogram as ex
        binWidth <- findBinWidth(.pd[[x]])
        veupathUtils::logWithTime('Determined ideal bin width.', verbose)
      }
      binSlider <- findBinSliderValues(.pd[[x]], xType, binWidth, 'binWidth')
  
      if (xType %in% c('NUMBER', 'INTEGER')) {
        binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(binWidth))
      } else {
        numericBinWidth <- as.numeric(gsub("[^0-9.-]", "", binWidth))
        if (is.na(numericBinWidth)) { numericBinWidth <- 1 }
        unit <- veupathUtils::trim(gsub("^[[:digit:]].", "", binWidth))
        binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(numericBinWidth), 'units'=jsonlite::unbox(unit))
      }
      veupathUtils::logWithTime('Determined bin width slider min, max and step values.', verbose)
    }
    attr$binSlider <- binSlider
    attr$binSpec <- binSpec
  } else {
    if (!is.null(binWidth)) {
      warning("X-axis must be a continuous number or date in order to be binned. Ignoring `binWidth`.")
    }
    if (!is.null(viewport)) {
      warning("X-axis must be a continuous number or date to apply a viewport range. Ignoring `viewport`.")
    }
  }
  
  # TODO unit tests for ordinal x-axis
  if (value == 'mean') {
    
    mean <- binMean(.pd, x, y, group, panel, binWidth, viewport, errorBars, xType)
    data.table::setnames(mean, c('binLabel', 'value'), c('seriesX', 'seriesY'))
    .pd <- mean
    veupathUtils::logWithTime('Mean calculated per X-axis value.', verbose)

  } else if (value == 'median') {

    median <- binMedian(.pd, x, y, group, panel, binWidth, viewport, errorBars, xType)
    data.table::setnames(median, c('binLabel', 'value'), c('seriesX', 'seriesY'))
    .pd <- median
    veupathUtils::logWithTime('Median calculated per X-axis value.', verbose)

  } else if (value == 'proportion') {

    proportion <- binCategoryProportion(.pd, x, y, group, panel, binWidth, viewport, errorBars, numeratorValues, denominatorValues, xType)
    data.table::setnames(proportion, c('binLabel', 'value'), c('seriesX', 'seriesY'))
    .pd <- proportion
    veupathUtils::logWithTime('Y-axis category proportions calculated per X-axis value.', verbose)

  }

  .pd$seriesY <- lapply(.pd$seriesY, as.character)
  attr$names <- names(.pd)

  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateLinePD <- function(.line, verbose) {
  xAxisVariable <- attr(.line, 'xAxisVariable')
  if (!xAxisVariable$dataShape %in% c('CONTINUOUS','ORDINAL')) {
    stop('The independent axis must be continuous or ordinal for lineplot.')
  }
  veupathUtils::logWithTime('Line plot request has been validated!', verbose)

  return(.line)
}

#' Line Plot as data.table
#'
#' This function returns a data.table of  
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' line plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete cases 
#' for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data 
#' for the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable 
#' sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 
#' 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param value character indicating whether to calculate 'mean', 'median', 'proportion' for y-axis
#' @param errorBars boolean indicating if we want 95% confidence intervals per x-axis tick
#' @param viewport List of min and max values to consider as the range of data
#' @param numeratorValues character vector of values from the y-axis variable to consider the numerator
#' @param denominatorValues character vector of values from the y-axis variable to consider the denominator
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of 
#' variables should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(1:20, 100, replace=T),
#'                  'yvar' = rnorm(100), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER'),
#'                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- lineplot.dt(df, map, value = 'median')
#' @export
lineplot.dt <- function(data, 
                         map, 
                         binWidth = NULL, 
                         value = c('mean',
                                   'median',
                                   'proportion'),
                         errorBars = c(TRUE, FALSE),
                         viewport = NULL,
                         numeratorValues = NULL,
                         denominatorValues = NULL,
                         evilMode = c(FALSE, TRUE),
                         collectionVariablePlotRef = NULL,
                         computedVariableMetadata = NULL,
                         verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  errorBars <- veupathUtils::matchArg(errorBars)
  evilMode <- veupathUtils::matchArg(evilMode) 
  verbose <- veupathUtils::matchArg(verbose)  

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  map <- validateMap(map)
  veupathUtils::logWithTime('Map has been validated.', verbose)

  # If there is a duplicated plotRef in map, it must match collectionVariablePlotRef
  if (any(duplicated(map$plotRef))) {
    if (!identical(collectionVariablePlotRef, unique(map$plotRef[duplicated(map$plotRef)]))) {
      stop('collectionVar error: duplicated map plotRef does not match collectionVariablePlotRef.')
    }
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type line.")
  }
  yAxisVariable <- plotRefMapToList(map, 'yAxisVariable')
  if (is.null(yAxisVariable$variableId)) {
    if (is.null(collectionVariablePlotRef)) {
      stop("Must provide yAxisVariable for plot type line.")
    }
  }
  overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariable2 <- plotRefMapToList(map, 'facetVariable2')

  # Handle collectionVars
  collectionVariableDetails <- list('inferredVariable' = NULL,
                         'inferredVarPlotRef' = 'yAxisVariable',
                         'collectionVariablePlotRef' = collectionVariablePlotRef)
  if (!is.null(collectionVariablePlotRef)) {
    if (identical(collectionVariablePlotRef, 'overlayVariable')) { 
      inferredVarEntityId <- unique(overlayVariable$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable1')) { 
      inferredVarEntityId <- unique(facetVariable1$entityId)
    } else if (identical(collectionVariablePlotRef, 'facetVariable2')) { 
      inferredVarEntityId <- unique(facetVariable2$entityId)
    } else { 
      stop('collectionVar error: collectionVariablePlotRef must be either overlayVariable, facetVariable1, or facetVariable2 for line.')
    }

    collectionVariableDetails$inferredVariable <- list('variableId' = 'yAxisVariable',
                                          'entityId' = inferredVarEntityId,
                                          'dataType' = 'NUMBER',
                                          'dataShape' = 'CONTINUOUS')

    veupathUtils::logWithTime('Created inferred variable from collectionVariable.', verbose)
  }

  .line <- newLinePD(.dt = data,
                            xAxisVariable = xAxisVariable,
                            yAxisVariable = yAxisVariable,
                            overlayVariable = overlayVariable,
                            facetVariable1 = facetVariable1,
                            facetVariable2 = facetVariable2,
                            viewport = viewport,
                            numeratorValues = numeratorValues,
                            denominatorValues = denominatorValues,
                            binWidth,
                            value = value,
                            errorBars = errorBars,
                            evilMode = evilMode,
                            collectionVariableDetails = collectionVariableDetails,
                            computedVariableMetadata = computedVariableMetadata,
                            verbose = verbose)

  .line <- validateLinePD(.line, verbose)
  veupathUtils::logWithTime(paste('New line plot object created with parameters viewport =', viewport, 
                                                                             ', binWidth =', binWidth, 
                                                                             ', value =', value, 
                                                                             ', errorBars =', errorBars, 
                                                                             ', evilMode =', evilMode, 
                                                                             ', numeratorValues = ', numeratorValues, 
                                                                             ', denominatorValues = ', denominatorValues, 
                                                                             ', verbose = ', verbose), verbose)

  return(.line)
}

#' Line Plot data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per group (per panel). Columns 
#' 'seriesX' and 'seriesY' contain the raw data for the 
#' line plot. Column 'group' and 'panel' specify the group the 
#' series data belongs to.
#' 
#' @section Evil Mode:
#' An `evilMode` exists. It will do the following: \cr
#' - return 'No data' as a regular value for strata vars but will discard incomplete 
#' cases for the axes vars \cr
#' - not return statsTables \cr
#' - allow smoothed means and agg values etc over axes values where we have no data for 
#' the strata vars \cr
#' - return a total count of plotted incomplete cases \cr
#' - represent missingness poorly, conflate the stories of completeness and missingness, 
#' mislead you and steal your soul \cr
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId 
#' and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 
#' 'overlayVariable', 'facetVariable1' and 'facetVariable2'
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param value character indicating whether to calculate 'mean', 'median', 'proportion' for y-axis
#' @param errorBars boolean indicating if we want 95% confidence intervals per x-axis tick
#' @param viewport List of min and max values to consider as the range of data
#' @param numeratorValues character vector of values from the y-axis variable to consider the numerator
#' @param denominatorValues character vector of values from the y-axis variable to consider the denominator
#' @param evilMode boolean indicating whether to represent missingness in evil mode.
#' @param collectionVariablePlotRef string indicating the plotRef to be considered as a collectionVariable. 
#' Accepted values are 'overlayVariable' and 'facetVariable1'. Required whenever a set of variables 
#' should be interpreted as a collectionVariable.
#' @param computedVariableMetadata named list containing metadata about a computed variable(s) involved in a plot. 
#' Metadata can include 'displayName', 'displayRangeMin', 'displayRangeMax', and 'collectionVariable'. Will be included as an attribute of the returned plot object.
#' @param verbose boolean indicating if timed logging is desired
#' @return character name of json file containing plot-ready data
#' @examples
#' # Construct example data
#' df <- data.table('xvar' = sample(1:20, 100, replace=T),
#'                  'yvar' = rnorm(100), stringsAsFactors = F)
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('xvar', 'yvar'),
#'                  'plotRef' = c('xAxisVariable', 'yAxisVariable'),
#'                  'dataType' = c('NUMBER', 'NUMBER'),
#'                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
#' 
#' # Returns the name of a json file
#' lineplot(df, map, value = 'median')
#' @export
lineplot <- function(data,
                      map,
                      binWidth = NULL,
                      value = c('mean', 
                                'median',
                                'proportion'),
                      errorBars = c(TRUE, FALSE),
                      viewport = NULL,
                      numeratorValues = NULL,
                      denominatorValues = NULL,
                      evilMode = c(FALSE, TRUE),
                      collectionVariablePlotRef = NULL,
                      computedVariableMetadata = NULL,
                      verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .line <- lineplot.dt(data,
                           map,
                           binWidth,
                           value = value,
                           errorBars = errorBars,
                           viewport = viewport,
                           numeratorValues = numeratorValues,
                           denominatorValues = denominatorValues,
                           evilMode = evilMode,
                           collectionVariablePlotRef = collectionVariablePlotRef,
                           computedVariableMetadata = computedVariableMetadata,
                           verbose = verbose)
                           
  outFileName <- writeJSON(.line, evilMode, 'lineplot', verbose)

  return(outFileName)
}

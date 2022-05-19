# TODO think about refactor after b57. this viewport is geolocation specific
# is this really a 'map-marker' class ?
newMapMarkersPD <- function(.dt = data.table::data.table(),
                         xAxisVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         geoAggregateVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         latitudeVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),
                         longitudeVariable = list('variableId' = NULL,
                                              'entityId' = NULL,
                                              'dataType' = NULL,
                                              'dataShape' = NULL,
                                              'displayLabel' = NULL),                     
                         value = character(),
                         binWidth,
                         binReportValue = character(),
                         viewport = list('latitude'=list('xMin'=NULL,
                                                         'xMax'=NULL),
                                         'longitude'=list('left'=NULL,
                                                          'right'=NULL)),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     xAxisVariable = xAxisVariable,
                     geoAggregateVariable = geoAggregateVariable,
                     latitudeVariable = latitudeVariable,
                     longitudeVariable = longitudeVariable,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "mapMarkers")

  attr <- attributes(.pd)

  x <- veupathUtils::toColNameOrNull(attr$xAxisVariable)
  xType <- attr$xAxisVariable$dataType
  geo <- veupathUtils::toColNameOrNull(attr$geoAggregateVariable)
  lat <- veupathUtils::toColNameOrNull(attr$latitudeVariable)
  lon <- veupathUtils::toColNameOrNull(attr$longitudeVariable)

  if (!length(.pd[[x]])) {
    rankedValues <- c('')
    binSlider <- list('min'=jsonlite::unbox(NA), 'max'=jsonlite::unbox(NA), 'step'=jsonlite::unbox(NA))
    binSpec <- list('type'=jsonlite::unbox(binReportValue), 'value'=jsonlite::unbox(NA))
    veupathUtils::logWithTime('No complete cases found.', verbose)
    attr$rankedValues <- rankedValues
    attr$binSlider <- binSlider
    attr$binSpec <- binSpec
  } else {
    if (xType == 'STRING') {
      ranked <- .pd[, .N, by=x]
      data.table::setorderv(ranked, c("N"),-1)
      rankedValues <- ranked[[x]]
      if (length(rankedValues) > 8) {
        rankedValues <- c(rankedValues[1:7], 'Other')
        .pd[[x]][!.pd[[x]] %in% rankedValues] <- 'Other'
      }
      attr$rankedValues <- rankedValues
    } else {
      if (binReportValue == 'binWidth') {
        if (xType %in% c('NUMBER', 'INTEGER')) {
          binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(binWidth))
        } else {
          numericBinWidth <- as.numeric(gsub("[^0-9.-]", "", binWidth))
          if (is.na(numericBinWidth)) { numericBinWidth <- 1 }
            unit <- veupathUtils::trim(gsub("^[[:digit:]].", "", binWidth))
            binSpec <- list('type'=jsonlite::unbox('binWidth'), 'value'=jsonlite::unbox(numericBinWidth), 'units'=jsonlite::unbox(unit))
          }
      } else {
        numBins <- binWidthToNumBins(.pd[[x]], binWidth)
        veupathUtils::logWithTime('Converted provided bin width to number of bins.', verbose)
        binSpec <- list('type'=jsonlite::unbox('numBins'), 'value'=jsonlite::unbox(numBins))
      }
      binSlider <- findBinSliderValues(.pd[[x]], xType, binWidth, binReportValue, 20)
      veupathUtils::logWithTime('Determined bin width slider min, max and step values.', verbose)
      attr$binSpec <- binSpec
      attr$binSlider <- binSlider

      xRange <- findViewport(.pd[[x]], xType)
      .pd[[x]] <- as.character(bin(.pd[[x]], binWidth, xRange))
      veupathUtils::logWithTime('Successfully binned continuous x-axis.', verbose)
    }
  }

  if (is.null(viewport)) {
    viewport <- findGeolocationViewport(.pd, lat, lon)
    veupathUtils::logWithTime('Determined default viewport.', verbose)
  } else {
    viewport <- validateGeolocationViewport(viewport, verbose)
  }
  if (is.null(viewport)) {
    attr$viewport <- list('latitude'=list('xMin'=NA,
                                          'xMax'=NA),
                          'longitude'=list('left'=NA,
                                           'right'=NA))
  } else {
    attr$viewport <- viewport
  }
  attr$viewport$latitude <- lapply(attr$viewport$latitude, jsonlite::unbox)
  attr$viewport$longitude <- lapply(attr$viewport$longitude, jsonlite::unbox)
  .pd <- filterToGeolocationViewport(.pd, lat, lon, viewport)

  if (value == 'count' ) {
    .pd$dummy <- 1
    .pd <- groupSize(.pd, x, 'dummy', NULL, NULL, geo, collapse = T)  
    veupathUtils::logWithTime('Value is set to `count`. Resulting mapMarker object will represent counts of unique x-axis values per panel.', verbose)
  } else if (value == 'proportion') {
    .pd$dummy <- 1
    .pd <- groupProportion(.pd, x, 'dummy', NULL, NULL, geo, 'group', collapse = T)
    veupathUtils::logWithTime('Value is set to `proportion`. Resulting mapMarker object will represent the relative proportions of unique xAxis values across panels.', verbose)
  }
  data.table::setnames(.pd, c(geo, 'label', 'value'))
  attr$names <- names(.pd)
  
  veupathUtils::setAttrFromList(.pd, attr)

  return(.pd)
}

validateGeolocationViewport <- function(viewport, verbose) {
  if (!is.list(viewport)) {
    return(FALSE)
  } else{
    if (!all(c('latitude', 'longitude') %in% names(viewport))) {
      return(FALSE)
    } else {
      if (!is.list(viewport$latitude) && !is.list(viewport$longitude)) {
        return(FALSE)
      } else{
        if (!all(c('xMin','xMax') %in% names(viewport$latitude)) &&
            !all(c('left','right') %in% names(viewport$longitude))) {
            return(FALSE)
        }
      }
    }
  }

  viewport$latitude$xMin <- as.numeric(viewport$latitude$xMin)
  viewport$latitude$xMax <- as.numeric(viewport$latitude$xMax)
  viewport$longitude$left <- as.numeric(viewport$longitude$left)
  viewport$longitude$right <- as.numeric(viewport$longitude$right)
  veupathUtils::logWithTime('Provided geolocation viewport validated.', verbose)

  return(viewport)
}

validateMapMarkersPD <- function(.map, verbose) {
  veupathUtils::logWithTime('MapMarkers request has been validated!', verbose)

  return(.map)
}

#' Map Markers as data.table
#'
#' This function returns a data.table of 
#' plot-ready data with one row per panel. Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'panel' specifies the facet panel the series data belongs to.
#' There are two options to calculate y-values for plotting. \cr
#' 1) 'count' occurrences of values from data.table input \cr 
#' 2) 'proportion' of occurrences of values from data.table input \cr 
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
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr
#' @section Geolocation Viewport Structure:
#' This is a list of lists taking the form: \cr
#' *latitude \cr
#' **xMin = numeric \cr
#' **xMax = numeric \cr
#' *longitude \cr
#' **left = numeric \cr
#' **right = numeric \cr
#' @return data.table plot-ready data
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. See section below for organization.
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param viewport List of values indicating the visible range of data
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xAxis' = sample(c('a','b','c'), 100, replace=T),
#'                  'facet' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('facet', 'xAxis'),
#'                  'plotRef' = c('geoAggregateVariable', 'xAxisVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#' 
#' # Returns a data table with plot-ready data
#' dt <- mapMarkers.dt(df,map,value='count')
#' @export
mapMarkers.dt <- function(data, 
                   map,
                   binWidth = NULL,
                   value = c('count', 'proportion'),
                   binReportValue = c('binWidth', 'numBins'),
                   viewport = NULL,  
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)
  binReportValue <- veupathUtils::matchArg(binReportValue)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xAxisVariable <- plotRefMapToList(map, 'xAxisVariable')
  # if we didnt require this, itd just return counts unstratified and could replace the java map plugin?
  if (is.null(xAxisVariable$variableId)) {
    stop("Must provide xAxisVariable for plot type mapMarkers.")
  }
  if (is.null(binWidth) && xAxisVariable$dataType != 'STRING') {
    x <- veupathUtils::toColNameOrNull(xAxisVariable)
    binWidth <- numBinsToBinWidth(data[[x]],8)
  }

  #now that this is map specific, should these all be required?
  geoAggregateVariable <- plotRefMapToList(map, 'geoAggregateVariable')
  latitudeVariable <- plotRefMapToList(map, 'latitudeVariable')
  longitudeVariable <- plotRefMapToList(map, 'longitudeVariable')

  .map <- newMapMarkersPD(.dt = data,
                    xAxisVariable = xAxisVariable,
                    geoAggregateVariable = geoAggregateVariable,
                    latitudeVariable = latitudeVariable,
                    longitudeVariable = longitudeVariable,
                    value = value,
                    binWidth = binWidth,
                    binReportValue = binReportValue,
                    viewport = viewport,
                    evilMode = evilMode,
                    verbose = verbose)

  .map <- validateMapMarkersPD(.map, verbose)
  veupathUtils::logWithTime(paste('New mapMarkers object created with parameters value =', value, ', binWidth =', binWidth, ', binReportValue =', binReportValue, ', viewport =', viewport, ', evilMode =', evilMode, ', verbose =', verbose), verbose)

  return(.map)
}

#' Map Markers data file
#'
#' This function returns the name of a json file containing 
#' plot-ready data with one row per panel. Columns 
#' 'label' and 'value' contain the raw data for plotting. Column 
#' 'panel' specifies the facet panel the series data belongs to.
#' There are two options to calculate y-values for plotting. \cr
#' 1) 'count' occurrences of values from data.table input \cr 
#' 2) 'proportion' of occurrences of values from data.table input \cr
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
#' @section Map Structure:
#' The 'map' associates columns in the data with plot elements, as well as passes information about each variable relevant for plotting. Specifically, the `map` argument is a data.frame with the following columns: \cr
#' - id: the variable name. Must match column name in the data exactly. \cr
#' - plotRef: The plot element to which that variable will be mapped. Options are 'xAxisVariable', 'yAxisVariable', 'zAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'.  \cr
#' - dataType: Options are 'NUMBER', 'INTEGER', 'STRING', or 'DATE'. Optional. \cr
#' - dataShape: Options are 'CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY. Optional. \cr 
#' @section Geolocation Viewport Structure:
#' This is a list of lists taking the form: \cr
#' *latitude \cr
#' **xMin = numeric \cr
#' **xMax = numeric \cr
#' *longitude \cr
#' **left = numeric \cr
#' **right = numeric \cr
#' @param data data.frame to make plot-ready data for
#' @param map data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot.
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param viewport List of values indicating the visible range of data
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('xAxis' = sample(c('a','b','c'), 100, replace=T),
#'                  'facet' = sample(c('red','green','blue'), 100, replace=T))
#' 
#' # Create map that specifies variable role in the plot and supplies variable metadata
#' map <- data.frame('id' = c('facet', 'xAxis'),
#'                  'plotRef' = c('geoAggregateVariable', 'xAxisVariable'),
#'                  'dataType' = c('STRING', 'STRING'),
#'                  'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#'
#' # Returns the name of a json file
#' mapMarkers(df,map,value='count')
#' @return character name of json file containing plot-ready data
#' @export
mapMarkers <- function(data, 
                map,
                binWidth = NULL,
                value = c('count', 'proportion'),
                binReportValue = c('binWidth', 'numBins'),
                viewport = NULL,
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .map <- map.dt(data, map, binWidth, value, binReportValue, viewport, evilMode, verbose)
  outFileName <- writeJSON(.map, evilMode, 'mapMarkers', verbose)

  return(outFileName)
}

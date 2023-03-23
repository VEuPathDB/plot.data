#' importFrom stringi stri_sort
newMapMarkersPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),                 
                         value = character(),
                         geolocationViewport = list('latitude'=list('xMin'=NULL,
                                                         'xMax'=NULL),
                                         'longitude'=list('left'=NULL,
                                                          'right'=NULL)),
                         xValues = veupathUtils::BinList(),
                         sampleSizes = logical(),
                         completeCases = logical(),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     sampleSizes = sampleSizes,
                     completeCases = completeCases,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "mapMarkers")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  geo <- veupathUtils::findColNamesFromPlotRef(variables, 'geo')
  lat <- veupathUtils::findColNamesFromPlotRef(variables, 'latitude')
  lon <- veupathUtils::findColNamesFromPlotRef(variables, 'longitude')

  # for the others this happen in the parent class, but color is different here
  xNeedsValues <- data.table::uniqueN(.pd[[x]]) > 8
  if (is.null(xValues) && xNeedsValues) {
    stop("Must provide values of interest for high cardinality or continuous map marker variables.")
  }
  .pd[[x]] <- recodeValues(.pd[[x]], xValues)

  if (is.null(geolocationViewport)) {
    geolocationViewport <- findGeolocationViewport(.pd, lat, lon)
    veupathUtils::logWithTime('Determined default viewport.', verbose)
  } else {
    geolocationViewport <- validateGeolocationViewport(geolocationViewport, verbose)
  }
  if (is.null(geolocationViewport)) {
    attr$viewport <- list('latitude'=list('xMin'=NA,
                                          'xMax'=NA),
                          'longitude'=list('left'=NA,
                                           'right'=NA))
  } else {
    attr$viewport <- geolocationViewport
  }
  attr$viewport$latitude <- lapply(attr$viewport$latitude, jsonlite::unbox)
  attr$viewport$longitude <- lapply(attr$viewport$longitude, jsonlite::unbox)
  .pd <- filterToGeolocationViewport(.pd, lat, lon, geolocationViewport)

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

validateGeolocationViewport <- function(geolocationViewport, verbose) {
  if (!is.list(geolocationViewport)) {
    return(FALSE)
  } else{
    if (!all(c('latitude', 'longitude') %in% names(geolocationViewport))) {
      return(FALSE)
    } else {
      if (!is.list(geolocationViewport$latitude) && !is.list(geolocationViewport$longitude)) {
        return(FALSE)
      } else{
        if (!all(c('xMin','xMax') %in% names(geolocationViewport$latitude)) &&
            !all(c('left','right') %in% names(geolocationViewport$longitude))) {
            return(FALSE)
        }
      }
    }
  }

  geolocationViewport$latitude$xMin <- as.numeric(geolocationViewport$latitude$xMin)
  geolocationViewport$latitude$xMax <- as.numeric(geolocationViewport$latitude$xMax)
  geolocationViewport$longitude$left <- as.numeric(geolocationViewport$longitude$left)
  geolocationViewport$longitude$right <- as.numeric(geolocationViewport$longitude$right)
  veupathUtils::logWithTime('Provided geolocation viewport validated.', verbose)

  return(geolocationViewport)
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
#' @param variables veupathUtils::VariableMetadataList
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param viewport List of values indicating the visible range of data
#' @param xValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.facet' = sample(c('red','green','blue'), 100, replace=T))
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
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'facet', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'geo'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns a data table with plot-ready data
#' dt <- mapMarkers.dt(df,map,value='count')
#' @export
mapMarkers.dt <- function(data, 
                   variables,
                   value = c('count', 'proportion'),
                   viewport = NULL,  
                   xValues = NULL,
                   sampleSizes = c(TRUE, FALSE),
                   completeCases = c(TRUE, FALSE),
                   evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                   verbose = c(TRUE, FALSE)) {

  value <- veupathUtils::matchArg(value)
  sampleSizes <- veupathUtils::matchArg(sampleSizes)
  completeCases <- veupathUtils::matchArg(completeCases)
  evilMode <- veupathUtils::matchArg(evilMode)
  verbose <- veupathUtils::matchArg(verbose)

  if (!'data.table' %in% class(data)) {
    data.table::setDT(data)
  }

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  # if we didnt require this, itd just return counts unstratified and could replace the java map plugin?
  if (is.null(xVM)) {
    stop("Must provide xAxisVariable for plot type mapMarkers.")
  }

  geoVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'geo')
  if (is.null(geoVM)) {
    stop("Must provide geoAggregateVariable for plot type mapMarkers.")
  }
  latitudeVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'latitude')
  if (is.null(latitudeVM) && !is.null(viewport)) {
    stop("Must provide latitudeVariable for plot type mapMarkers.")
  }
  longitudeVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'longitude')
  if (is.null(longitudeVM) && !is.null(viewport)) {
    stop("Must provide longitudeVariable for plot type mapMarkers.")
  }

  .map <- newMapMarkersPD(.dt = data,
                    variables = variables,
                    value = value,
                    geolocationViewport = viewport,
                    xValues = xValues,
                    sampleSizes = sampleSizes,
                    completeCases = completeCases,
                    evilMode = evilMode,
                    verbose = verbose)

  .map <- validateMapMarkersPD(.map, verbose)
  veupathUtils::logWithTime(paste('New mapMarkers object created with parameters value =', value,
                                                                              ',viewport =', viewport,
                                                                              ', sampleSizes = ', sampleSizes,
                                                                              ', completeCases = ', completeCases,
                                                                              ', evilMode =', evilMode,
                                                                              ', verbose =', verbose), verbose)

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
#' @section Geolocation Viewport Structure:
#' This is a list of lists taking the form: \cr
#' *latitude \cr
#' **xMin = numeric \cr
#' **xMax = numeric \cr
#' *longitude \cr
#' **left = numeric \cr
#' **right = numeric \cr
#' @param data data.frame to make plot-ready data for
#' @param variables veupathUtils::VariableMetadataList
#' @param value String indicating how to calculate y-values ('count', 'proportion')
#' @param viewport List of values indicating the visible range of data
#' @param xValues veupathUtils::BinList providing overlay values of interest
#' @param sampleSizes boolean indicating if sample sizes should be computed
#' @param completeCases boolean indicating if complete cases should be computed
#' @param evilMode String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables') 
#' @param verbose boolean indicating if timed logging is desired
#' @examples
#' # Construct example data
#' df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
#'                  'entity.facet' = sample(c('red','green','blue'), 100, replace=T))
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
#'     variableSpec = veupathUtils::VariableSpec(variableId = 'facet', entityId = 'entity'),
#'     plotReference = veupathUtils::PlotReference(value = 'geo'),
#'     dataType = veupathUtils::DataType(value = 'STRING'),
#'     dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
#'   )
#' )
#' 
#' # Returns the name of a json file
#' mapMarkers(df,map,value='count')
#' @return character name of json file containing plot-ready data
#' @export
mapMarkers <- function(data, 
                variables,
                value = c('count', 'proportion'),
                viewport = NULL,
                xValues = NULL,
                sampleSizes = c(TRUE, FALSE),
                completeCases = c(TRUE, FALSE),
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .map <- mapMarkers.dt(data = data,
                        variables = variables,
                        value = value,
                        viewport = viewport,
                        sampleSizes = sampleSizes,
                        completeCases = completeCases,
                        evilMode = evilMode,
                        verbose = verbose)

  outFileName <- writeJSON(.map, evilMode, 'mapMarkers', verbose)

  return(outFileName)
}

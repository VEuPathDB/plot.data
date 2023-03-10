#' importFrom stringi stri_sort
newMapMarkersPD <- function(.dt = data.table::data.table(),
                         variables = veupathUtils::VariableMetadataList(),                 
                         value = character(),
                         binWidth,
                         binReportValue = character(),
                         binRange,
                         geolocationViewport = list('latitude'=list('xMin'=NULL,
                                                         'xMax'=NULL),
                                         'longitude'=list('left'=NULL,
                                                          'right'=NULL)),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  .pd <- newPlotdata(.dt = .dt,
                     variables = variables,
                     evilMode = evilMode,
                     verbose = verbose,
                     class = "mapMarkers")

  attr <- attributes(.pd)
  variables <- attr$variables

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  xType <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  geo <- veupathUtils::findColNamesFromPlotRef(variables, 'geo')
  lat <- veupathUtils::findColNamesFromPlotRef(variables, 'latitude')
  lon <- veupathUtils::findColNamesFromPlotRef(variables, 'longitude')

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

  if (!length(.pd[[x]])) {
    rankedValues <- c('')
    # these basically empty `binSlider` and `binSpec` attributes are causing problems for the client
    # the raml and client specs says they are optional.  Let's leave them out.
    # binSlider <- list('min'=jsonlite::unbox(NA), 'max'=jsonlite::unbox(NA), 'step'=jsonlite::unbox(NA))
    # binSpec <- list('type'=jsonlite::unbox(binReportValue), 'value'=jsonlite::unbox(NA))
    veupathUtils::logWithTime('No complete cases found.', verbose)
    attr$rankedValues <- rankedValues
    attr$overlayValues <- rankedValues
    # attr$binSlider <- binSlider
    # attr$binSpec <- binSpec
  } else {
    if (xType == 'STRING') {
      ranked <- .pd[, .N, by=x]
      data.table::setorderv(ranked, c("N"),-1)
      rankedValues <- ranked[[x]]
      if (length(rankedValues) > 8) {
        rankedValues <- c(rankedValues[1:7], 'Other')
        .pd[[x]][!.pd[[x]] %in% rankedValues] <- 'Other'
        overlayValues <- c(stringi::stri_sort(rankedValues[1:7], numeric=TRUE), 'Other')
      } else {
        overlayValues <- stringi::stri_sort(rankedValues, numeric=TRUE)
      }
      attr$rankedValues <- rankedValues
      attr$overlayValues <- overlayValues
    } else {
      if (is.null(binRange)) {
        xRange <- findViewport(.pd[[x]], xType)
      } else {
        xRange <- validateBinRange(.pd[[x]], binRange, xType, verbose)
      }
      xVP <- adjustToViewport(.pd[[x]], xRange)

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
        numBins <- binWidthToNumBins(xVP, binWidth)
        veupathUtils::logWithTime('Converted provided bin width to number of bins.', verbose)
        binSpec <- list('type'=jsonlite::unbox('numBins'), 'value'=jsonlite::unbox(numBins))
      }
      binSlider <- findBinSliderValues(xVP, xType, binWidth, binReportValue, 20)
      veupathUtils::logWithTime('Determined bin width slider min, max and step values.', verbose)
      attr$binSpec <- binSpec
      attr$binSlider <- binSlider

      .pd[[x]] <- bin(.pd[[x]], binWidth, xRange, stringsAsFactors=TRUE)
      veupathUtils::logWithTime('Successfully binned continuous x-axis.', verbose)
      
      overlayValues <- levels(.pd[[x]])
      .pd[[x]] <- as.character(.pd[[x]])
      attr$overlayValues <- overlayValues

      # maybe worth at some point a fxn getRankedValues(x = character(), maxNumValues = integer(), otherBin = c(T,F))
      # if maxNumValues was exceeded and otherBin = F then it would just put out an error
      # maxNumValues could be NULL by default maybe?
      # would something like that be better here or veupathUtils (could mbio use it?)
      ranked <- .pd[, .N, by=x]
      data.table::setorderv(ranked, c("N"),-1)
      rankedValues <- ranked[[x]]
      attr$rankedValues <- rankedValues
    }
  }

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

#' @export
validateBinRange <- function(x, binRange, varType, verbose) {
  if (!is.list(binRange)) {
    stop("Invalid bin range provided: Not a list.")
  } else{
    if (!all(c('max', 'min') %in% names(binRange)) && !all(c('xMax', 'xMin') %in% names(binRange))) {
      stop("Invalid bin range provided: No min or max values found.")
    }
  }

  min <- ifelse(is.null(binRange$min), binRange$xMin, binRange$min)
  max <- ifelse(is.null(binRange$max), binRange$xMax, binRange$max)

  #not a viewport, shouldnt subset data range
  if (min > min(x) || max < max(x)) {
    stop("Invalid bin range provided: Bin range cannot represent a subset of the data range.")
  }

  if (varType %in% c('NUMBER', 'INTEGER')) {
    binRange$xMin <- as.numeric(min)
    binRange$xMax <- as.numeric(max)
  } else if (varType == 'DATE') {
    binRange$xMin <- as.Date(min, format='%Y-%m-%d')
    binRange$xMax <- as.Date(max, format='%Y-%m-%d')
  }
  binRange$min <- NULL
  binRange$max <- NULL
  veupathUtils::logWithTime('Provided bin range validated.', verbose)

  return(binRange)
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
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param binRange List of min and max values to bin the xAxisVariable over
#' @param viewport List of values indicating the visible range of data
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
                   binWidth = NULL,
                   value = c('count', 'proportion'),
                   binReportValue = c('binWidth', 'numBins'),
                   binRange = NULL,
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

  xVM <- veupathUtils::findVariableMetadataFromPlotRef(variables, 'xAxis')
  # if we didnt require this, itd just return counts unstratified and could replace the java map plugin?
  if (is.null(xVM)) {
    stop("Must provide xAxisVariable for plot type mapMarkers.")
  }
  if (is.null(binWidth) && xVM@dataType@value != 'STRING') {
    x <- veupathUtils::getColName(xVM@variableSpec)

    if (is.null(binRange)) {
      binWidth <- numBinsToBinWidth(data[[x]], 8, na.rm = TRUE)
    } else {
      xVals <- data[[x]][complete.cases(data[[x]])]
      xVP <- adjustToViewport(xVals, validateBinRange(xVals, binRange, xAxisVariable$dataType, FALSE))
      binWidth <- numBinsToBinWidth(xVP, 8)
    }
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
                    binWidth = binWidth,
                    binReportValue = binReportValue,
                    binRange = binRange,
                    geolocationViewport = viewport,
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
#' @param binWidth numeric value indicating width of bins, character (ex: 'year') if xaxis is a date
#' @param binReportValue String indicating if number of bins or bin width used should be returned
#' @param binRange List of min and max values to bin the xAxisVariable over
#' @param viewport List of values indicating the visible range of data
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
                binWidth = NULL,
                value = c('count', 'proportion'),
                binReportValue = c('binWidth', 'numBins'),
                binRange = NULL,
                viewport = NULL,
                evilMode = c('noVariables', 'allVariables', 'strataVariables'),
                verbose = c(TRUE, FALSE)) {

  verbose <- veupathUtils::matchArg(verbose)

  .map <- mapMarkers.dt(data, variables, binWidth, value, binReportValue, binRange, viewport, evilMode, verbose)
  outFileName <- writeJSON(.map, evilMode, 'mapMarkers', verbose)

  return(outFileName)
}

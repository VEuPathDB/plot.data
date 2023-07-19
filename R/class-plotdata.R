### evilMode will do the following:
###   - when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars.
###   - when `allVariables` it will return 'no data' as a regular value for all variables.
###   - when `noVariables` it will do the sensible thing and return complete cases only.
###   - not return statsTables
###   - allow smoothed means and agg values etc over axes values where we have no data for the strata vars
###   - return a total count of plotted incomplete cases
###   - represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul

#' @importFrom stats complete.cases
#' @importFrom purrr map
newPlotdata <- function(.dt = data.table(),
                         #make sure lat, lon, geoAgg vars are valid plot References
                         variables = NULL,    
                         useGradientColorscale = FALSE,    
                         overlayValues = veupathUtils::BinList(),            
                         sampleSizes = logical(),
                         completeCases = logical(),
                         inferredVarAxis = c('y', 'x'),
                         evilMode = character(),
                         verbose = logical(),
                         ...,
                         class = character()) {

  inferredVarAxis <- veupathUtils::matchArg(inferredVarAxis)

  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  xType <- veupathUtils::findDataTypesFromPlotRef(variables, 'xAxis')
  xShape <- veupathUtils::findDataShapesFromPlotRef(variables, 'xAxis')
  y <- veupathUtils::findColNamesFromPlotRef(variables, 'yAxis')
  yType <- veupathUtils::findDataTypesFromPlotRef(variables, 'yAxis')
  z <- veupathUtils::findColNamesFromPlotRef(variables, 'zAxis')
  zType <- veupathUtils::findDataTypesFromPlotRef(variables, 'zAxis')
  group <- veupathUtils::findColNamesFromPlotRef(variables, 'overlay')
  groupType <- veupathUtils::findDataTypesFromPlotRef(variables, 'overlay')
  # TODO decide if this can have the 1, 2 removed or if the PlotReference class needs to discriminate between facet1 and facet2
  facet1 <- veupathUtils::findColNamesFromPlotRef(variables, 'facet1')
  facet1Type <- veupathUtils::findDataTypesFromPlotRef(variables, 'facet1')
  facet2 <- veupathUtils::findColNamesFromPlotRef(variables, 'facet2')
  facet2Type <- veupathUtils::findDataTypesFromPlotRef(variables, 'facet2')
  # TODO add geo, lat, long values to PlotReference class
  geo <- veupathUtils::findColNamesFromPlotRef(variables, 'geo')

  #think the only thing we need to do w these at this point is make sure theyre included in the .pd 
  lat <- veupathUtils::findColNamesFromPlotRef(variables, 'latitude')
  lon <- veupathUtils::findColNamesFromPlotRef(variables, 'longitude')

  isEvil <- ifelse(evilMode %in% c('allVariables', 'strataVariables'), TRUE, FALSE)
  collectionVarMetadata <- veupathUtils::findCollectionVariableMetadata(variables)
  isOverlayCollection <- ifelse(is.null(collectionVarMetadata), FALSE, ifelse(collectionVarMetadata@plotReference@value == 'overlay', TRUE, FALSE)) 
  prefixMap <- list('x' = 'xAxis',
                      'y' = 'yAxis',
                      'group' = 'overlay',
                      'facet1' = 'facet1',
                      'facet2' = 'facet2')
  
  # Extract names of vars for which naToZero is TRUE
  # Note: if we want to change default behavior in the future, this predicate function is a good place to do it
  impute0cols <- veupathUtils::findColNamesByPredicate(variables, function(x) {ifelse(is.na(x@imputeZero), FALSE, x@imputeZero)})

  # Replace NAs with 0s if naToZero set
  if (!!length(impute0cols)) {
    impute0Cols <- veupathUtils::validateNumericCols(.dt, cols=impute0cols)
    veupathUtils::setNaToZero(.dt, cols=impute0cols)
    veupathUtils::logWithTime(paste('Replaced NA with 0 in the following columns: ', impute0cols), verbose)
  }

  ## Calculate complete cases table if desired
  if (completeCases) {
    #lat and lon must be used w a geohash, so they dont need to be part of completeCases*
    varCols <- c(x, y, z, group, facet1, facet2, geo)
    completeCasesTable <- data.table::setDT(lapply(.dt[, ..varCols], function(a) {sum(complete.cases(a))}))
    completeCasesTable <- data.table::transpose(completeCasesTable, keep.names = 'variableDetails')
    data.table::setnames(completeCasesTable, 'V1', 'completeCases')
    
    veupathUtils::logWithTime('Determined the number of complete cases per variable.', verbose)
  }
  
  isFacetCollection <- ifelse(is.null(collectionVarMetadata), FALSE, ifelse(collectionVarMetadata@plotReference@value %in% c('facet1', 'facet2'), TRUE, FALSE))
  if (!isFacetCollection) {
    panelData <- makePanels(.dt, facet1, facet2)
    .dt <- data.table::setDT(panelData[[1]])
    panel <- panelData[[2]]
    if (!is.null(panel)){
      if (uniqueN(.dt[[panel]]) > 25) stop("Maximum number of panels allowed is 25.")
    }
  } else {
    panel <- c(facet1, facet2)
  }

  myCols <- c(x, y, z, lat, lon, group, panel, geo)
  .dt <- .dt[, myCols, with=FALSE]
  veupathUtils::logWithTime('Identified facet intersections.', verbose)

  # Reshape data and remap variables if collectionVar is specified
  if (!is.null(collectionVarMetadata)) {    
    
    inferredVarMetadata <- veupathUtils::VariableMetadata(
                                variableClass = collectionVarMetadata@variableClass,
                                variableSpec = veupathUtils::VariableSpec(variableId = 'collectionVarValues', entityId = collectionVarMetadata@variableSpec@entityId),
                                plotReference = veupathUtils::PlotReference(value = prefixMap[names(prefixMap) == inferredVarAxis][[1]]),
                                displayName = paste(collectionVarMetadata@displayName, 'values'),
                                displayRangeMin = collectionVarMetadata@displayRangeMin,
                                displayRangeMax = collectionVarMetadata@displayRangeMax,
                                dataType = collectionVarMetadata@dataType,
                                dataShape = collectionVarMetadata@dataShape,
                                vocabulary = collectionVarMetadata@vocabulary,
                                imputeZero = collectionVarMetadata@imputeZero)

    collectionVarMetadata@dataType@value <- 'STRING'
    collectionVarMetadata@dataShape@value <- 'CATEGORICAL'
    collectionVarMetadata@displayRangeMin <- NA_real_
    collectionVarMetadata@displayRangeMax <- NA_real_
    collectionVarMetadata@imputeZero <- FALSE
    collectionVarMetadata@vocabulary <- unlist(lapply(as.list(collectionVarMetadata@members), function(x) {x@variableId}))

    collectionVarMemberColNames <- unlist(lapply(as.list(collectionVarMetadata@members), veupathUtils::getColName))
    collectionVarMetadata@isCollection = FALSE
    collectionVarMetadata@members <- veupathUtils::VariableSpecList()
 
    validObject(collectionVarMetadata)
    collectionVarIndex <- which(purrr::map(as.list(variables), function(x) {if (x@isCollection) {return(TRUE)}}) %in% TRUE)
    variables[[collectionVarIndex]] <- collectionVarMetadata
    variables[[length(variables) + 1]] <- inferredVarMetadata

    veupathUtils::logWithTime('Identified collection variable members and values.', verbose)
    if (evilMode == 'allVariables') stop('collectionVar error: evilMode = `allVariables` not compatible with collection variable')

    # Set variable, value names appropriately
    if (is.na(inferredVarMetadata@variableSpec@entityId)) {
      variable.name <- collectionVarPlotRef
      value.name <- inferredVarMetadata@variableSpec@variableId
    } else {
      variable.name <- veupathUtils::getColName(collectionVarMetadata@variableSpec)
      value.name <- veupathUtils::getColName(inferredVarMetadata@variableSpec)
    }

    # Calculate complete cases *before* reshaping the data
    if (completeCases) {
      # only count rows where we have at least one value in one of the collection var member columns
      completeCasesPerCollectionCol <- lapply(collectionVarMemberColNames, function(collectionVarMember) {return(complete.cases(.dt[, ..collectionVarMember]))})
      collectionVarDataRows <- Reduce("+", completeCasesPerCollectionCol) > 0  # Any row with val=0 means that row was missing for all vars in the collection and should be removed from the count
      # Columns not corresponding to a collection var are treated differently. Calculate their complete cases as normal
      nonCollectionVarColNames <- setdiff(c(x,y,z,group, panel), collectionVarMemberColNames)
      if (length(nonCollectionVarColNames) > 0) {
        nonCollectionVarDataRows <- complete.cases(.dt[, ..nonCollectionVarColNames])
        # Count the rows that we keep from the collection var complete cases *and* non-collection var complete cases
        completeCasesAllVars <- jsonlite::unbox(nrow(.dt[collectionVarDataRows*nonCollectionVarDataRows,]))
      } else {
        # If nonCollectionVarColNames is empty, it will interfere with the multiplication above and return 0 for complete cases always.
        # Instead, here we only use the collection vars to calculate the complete cases
        completeCasesAllVars <- jsonlite::unbox(nrow(.dt[collectionVarDataRows]))
      }

      if (collectionVarMetadata@plotReference@value == 'xAxis') {
        # Since we force the collection value to be the y variable, the whole collection includes x and y.
        completeCasesAxesVars <- jsonlite::unbox(nrow(.dt[collectionVarDataRows,]))
      } else {
        # Count rows with data for x and at least 1 collection variable (remember, collection values always map to y)
        axisDataRows <- complete.cases(.dt[, ..x]) * collectionVarDataRows
        completeCasesAxesVars <- jsonlite::unbox(nrow(.dt[axisDataRows,]))
      }
    }

    # Reshape data
    .dt <- data.table::melt(.dt, measure.vars = collectionVarMemberColNames,
                        variable.factor = FALSE,
                        variable.name= variable.name,
                        value.name=value.name)

    veupathUtils::logWithTime('Data reshaped according to collection variable.', verbose)

    # strip entityId from value
    .dt[[variable.name]] <- gsub(paste0(inferredVarMetadata@variableSpec@entityId, '.'), '', .dt[[variable.name]])

    .dt[[variable.name]] <- updateType(.dt[[variable.name]], collectionVarMetadata@dataType@value)
    prefix <- names(prefixMap)[prefixMap == collectionVarMetadata@plotReference@value]
    assign(prefix, veupathUtils::getColName(collectionVarMetadata@variableSpec))
    assign(paste0(prefix, 'Type'), collectionVarMetadata@dataType@value)
    assign(paste0(prefix, 'Shape'), collectionVarMetadata@dataShape@value)

    if (collectionVarMetadata@plotReference@value %in% c('facet1', 'facet2')) {
      panelData <- makePanels(.dt, facet1, facet2)
      .dt <- data.table::setDT(panelData[[1]])
      panel <- panelData[[2]]
      if (uniqueN(.dt[[panel]]) > 25) stop("Maximum number of panels allowed is 25.")
    }

    assign(inferredVarAxis, veupathUtils::getColName(inferredVarMetadata@variableSpec))
    assign(paste0(inferredVarAxis, 'Type'), veupathUtils::toStringOrNull(inferredVarMetadata@dataType@value))

    data.table::setcolorder(.dt, c(x, y, z, group, panel))

    veupathUtils::logWithTime('Handling of collectionVariables complete.', verbose)

  }

  # Update types
  .dt[[x]] <- updateType(.dt[[x]], xType)
  if (!is.null(y)) { .dt[[y]] <- updateType(.dt[[y]], yType) }
  if (!is.null(z)) { .dt[[z]] <- updateType(.dt[[z]], zType) }
  if (!is.null(geo)) { .dt[[geo]] <- updateType(.dt[[geo]], 'STRING')}
  if (!is.null(lat)) { .dt[[lat]] <- updateType(.dt[[lat]], 'NUMBER')}
  if (!is.null(lon)) { .dt[[lon]] <- updateType(.dt[[lon]], 'NUMBER')}
  if (!is.null(group)) { .dt[[group]] <- updateType(.dt[[group]], groupType) }
  if (!is.null(panel)) { .dt[[panel]] <- updateType(.dt[[panel]], 'STRING') }
  veupathUtils::logWithTime('Base data types updated for all columns as necessary.', verbose)

  if (!is.null(group)) {
    if (!isOverlayCollection) {
      groupNeedsOverlayValues <- data.table::uniqueN(.dt[[group]]) > 8 && useGradientColorscale == FALSE
      if (is.null(overlayValues) && groupNeedsOverlayValues) {
        stop("Must provide overlay values of interest for high cardinality or continuous overlay variables.")
      }
      .dt[[group]] <- recodeValues(.dt[[group]], overlayValues)
    } else {
      groupNeedsOverlayValues <- data.table::uniqueN(.dt[[get(inferredVarAxis)]]) > 10 && yType == "STRING"
      if (is.null(overlayValues) && groupNeedsOverlayValues) {
        stop("Must provide axis values of interest for high cardinality overlay variable collections.")
      }
      .dt[[get(inferredVarAxis)]] <- recodeValues(.dt[[y]], overlayValues)
    }
  }

  # TODO review logic here around complete cases on the panel column
  if (!exists('completeCasesAllVars') && completeCases) {
    completeCasesAllVars <- complete.cases(.dt[, c(x,y,z,group,panel,geo), with=FALSE])
    completeCasesAllVars <- jsonlite::unbox(nrow(.dt[completeCasesAllVars,]))
  }
  if (!exists('completeCasesAxesVars') && completeCases) {
    completeCasesAxesVars <- jsonlite::unbox(nrow(.dt[complete.cases(.dt[, c(x,y), with=FALSE]),]))
    veupathUtils::logWithTime('Determined total number of complete cases across axes and strata vars.', verbose)
  }

  if (isEvil) {
    # Assign NA strata values to 'No data', with the exception of continuous overlays which should stay numeric
    # TODO at some point i want to make sure our logic here is solid. 
    # is it possible for a continuous overlay to represent something other than color for ex? and what then?
    if (!is.null(group) && !identical(veupathUtils::findVariableMetadataFromPlotRef(variables, 'overlay')@dataShape@value,'CONTINUOUS')) { .dt[[group]][is.na(.dt[[group]])] <- 'No data' }
    if (!is.null(panel)) { .dt[[panel]][is.na(.dt[[panel]])] <- 'No data' }
    if (!is.null(geo)) { .dt[[geo]][is.na(.dt[[geo]])] <- 'No data'}
    if (evilMode == 'allVariables') {
      if (!is.null(x)) { .dt[[x]][is.na(.dt[[x]])] <- 'No data' }
      if (!is.null(y)) { .dt[[y]][is.na(.dt[[y]])] <- 'No data' }
      if (!is.null(z)) { .dt[[z]][is.na(.dt[[z]])] <- 'No data' }
    } else {
      axesCols <- c(x, y, z)
      axesDT <- .dt[, axesCols, with = FALSE]
      .dt <- .dt[complete.cases(axesDT)]
    }   
  } else { 
    .dt <- .dt[complete.cases(.dt),]
  }

  # If using a gradient colorscale, overlay var does not contribute to final groups
  overlayGroup <- if (useGradientColorscale) NULL else group

  # Calculate sample sizes if requested
  if (sampleSizes) {
    if (xShape != 'CONTINUOUS' || uniqueN(.dt[[x]]) < 9) {
      .dt$dummy <- 1
      sampleSizeTable <- groupSize(.dt, x=x, y="dummy", overlayGroup, panel, geo, collapse=F)
      .dt$dummy <- NULL
    } else {
      sampleSizeTable <- groupSize(.dt, x=NULL, y=x, overlayGroup, panel, geo, collapse=F)
    }
    veupathUtils::logWithTime('Calculated sample sizes per group.', verbose)
  }

  if (is.null(xType)) {
    index <- findVariableIndexByPlotRef(variables, 'xAxis')
    xIsNum = all(!is.na(as.numeric(.dt[[x]])))
    variables[[index]]@dataType@value <- 'NUMBER'
    xIsDate = !xIsNum && all(!is.na(as.Date(.dt[[x]], format='%Y-%m-%d')))
    variables[[index]]@dataType@value <- 'DATE'
    xIsChar = !xIsNum && !xIsDate && all(!is.na(as.character(.dt[[x]])))
    variables[[index]]@dataType@value <- 'STRING'
    validObject(variables[[index]])
  } 

  attr <- attributes(.dt)
  attr$variables <- variables
  if (completeCases) {
    attr$completeCasesAllVars <- completeCasesAllVars
    attr$completeCasesAxesVars <- completeCasesAxesVars
    attr$completeCasesTable <- completeCasesTable
  }
  if (sampleSizes) attr$sampleSizeTable <- collapseByGroup(sampleSizeTable, overlayGroup, panel, geo)
  attr$class = c(class, 'plot.data', attr$class)

  veupathUtils::setAttrFromList(.dt, attr)
  .pd <- validatePlotdata(.dt)
  veupathUtils::logWithTime('Base plot.data object created.', verbose)

  return(.pd)
}

validatePlotdata <- function(.pd) {
  .dt <- unclass(.pd)
  variables <- attr(.pd, 'variables')

  #we need at min an x axis var
  x <- veupathUtils::findColNamesFromPlotRef(variables, 'xAxis')
  if (is.null(x)) stop("All plot.data classes require at minimum an x-axis variable.")
  if (!x %in% names(.pd)) stop("Specified x-axis variable cannot be found in provided data frame.")
  
  #unique plot refs
  plotRefs <- unlist(lapply(as.list(variables), function(x) {x@plotReference@value}))
  if (length(plotRefs) != data.table::uniqueN(plotRefs)) stop("All PlotReferences must be unique in the provided VariableMetadataList.")

  # also check there is only one collection in variables
  collectionsCount <- sum(unlist(lapply(as.list(variables), function(x) {x@isCollection})))
  if (collectionsCount > 1) stop("More than one collection variable was specified.")

  class <- attr(.pd, 'class')
  stopifnot(is.character(class))

  return(.pd)
}

# Additional accessor functions
sampleSizeTable <- function(.pd) { attr(.pd, 'sampleSizeTable') }
completeCasesTable <- function(.pd) { attr(.pd, 'completeCasesTable') }
#these helpers need either validation or to be a dedicated method
statsTable <- function(.pd) { attr(.pd, 'statsTable') }
variablesList <- function(.pd) { as.list(attr(.pd, 'variables')) }

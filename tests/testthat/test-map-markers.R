context('mapMarkers')

test_that("mapMarkers.dt does not fail when there are no complete cases.", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- data.noneComplete[is.na(entity.binary1),]

  dt <- mapMarkers.dt(df, variables, value='count')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- mapMarkers.dt(df, variables, value='proportion')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)
})

test_that("mapMarkers.dt() returns a valid plot.data mapMarkers object", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mapMarkers')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','viewport'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 2)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.cat6','size'))
  expect_equal(nrow(sampleSizes), 4)


  # Ensure sampleSizeTable and completeCasesTable do not get returned if we do not ask for them.
  dt <- mapMarkers.dt(df, variables, value='count', sampleSizes = FALSE, completeCases = FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mapMarkers')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables','viewport'))

})


test_that("mapMarkers.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt$label, 'list')
  expect_equal(class(unlist(dt$label)), 'character')
  expect_is(dt$value, 'list')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer')) 
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat5)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("mapMarkers.dt() returns an appropriately sized data.table", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'label', 'value'))
  
  dt <- mapMarkers.dt(df, variables, value='proportion')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  # With factors
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),6)
  expect_equal(names(dt),c('entity.factor6', 'label', 'value'))
  expect_equal(class(dt$entity.factor6), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),6)
  expect_equal(names(dt),c('entity.factor6', 'label', 'value'))
  expect_equal(class(dt$entity.factor6), 'character')


  # With continuous x var (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- mapMarkers.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'label', 'value'))
  
  dt <- mapMarkers.dt(df, variables, value='proportion')
  expect_is(dt, 'data.table')
  expect_is(dt, 'mapMarkers')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)

  
})

test_that("mapMarkers() returns appropriately formatted json", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'longitude'),
      dataType = new("DataType", value = 'LONGITUDE'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'latitude'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)
  df$entity.int7 <- as.character(df$entity.int7)
  viewport <- list('latitude'=list('xMin'=-.5,
                                   'xMax'=.5),
                   'longitude'=list('left'=-.5,
                                    'right'=.5))

  dt <- mapMarkers.dt(df, variables, value='count', viewport=viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$mapMarkers$config$viewport)), 'numeric')
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cont6','cont5','cat5','int7'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'int7')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int7', 'cat5'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int11', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)
  df$entity.int11 <- as.character(df$entity.int11)

  makeCategoricalBin <- function(value) {veupathUtils::Bin('binLabel'=value)}
  overlayBins <- veupathUtils::BinList(S4Vectors::SimpleList(lapply(as.character(c(1,2,3,4,5,6,7)), makeCategoricalBin)))

  dt <- mapMarkers.dt(df, variables, value='count', xValues = overlayBins)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(jsonList$mapMarkers$data$label[[1]], c("1","2","3","4","5","6","7","__UNSELECTED__"))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$mapMarkers$config$viewport)), 'NULL')
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','int11'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'int11')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int11', 'cat5'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  overlayBins <- veupathUtils::getDiscretizedBins(df$entity.contA)

  dt <- mapMarkers.dt(df, variables, value='count', xValues = overlayBins)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(all(jsonList$mapMarkers$data$label[[5]] %in% unlist(lapply(overlayBins, function(x) {x@binLabel}))), TRUE)
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$mapMarkers$config$viewport)), 'NULL')
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','contA'))
  # dont have sample sizes for x bc it was cont for the parent class :(
  # map doesnt care so im not going to change it yet but stil...
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'cat5'))

  # Ensure sampleSizeTable and completeCasesTable are not part of json if we do not ask for them.
  dt <- mapMarkers.dt(df, variables, value='count', xValues = overlayBins, sampleSizes = FALSE, completeCases = FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$mapMarkers$config$viewport)), 'NULL')
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','contA'))
  

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mapMarkers.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','cat6'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat5'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "xLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mapMarkers.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','cat6'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$geoAggregateVariableDetails$variableId[[1]], 'cat5')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat5'))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mapMarkers.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers$data$geoAggregateVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$mapMarkers$data$geoAggregateVariableDetails$variableId[[1]], 'cat5')
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','cat6'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- mapMarkers.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('int7','int6'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'int7'))


  # With continuous x var (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- mapMarkers.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('mapMarkers','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mapMarkers), c('data','config'))
  expect_equal(names(jsonList$mapMarkers$data), c('geoAggregateVariableDetails','label','value'))
  expect_equal(names(jsonList$mapMarkers$config), c('variables','completeCasesAllVars','completeCasesAxesVars','viewport'))
  expect_equal(names(jsonList$mapMarkers$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$mapMarkers$config$variables$variableSpec$variableId, c('cat5','int6'))
  expect_equal(names(jsonList$sampleSizeTable), c('geoAggregateVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$geoAggregateVariableDetails$value[[1]]), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'int6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'cat5'))

})


test_that("mapMarkers.dt() returns correct information about missing data", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- mapMarkers.dt(df, variables, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- mapMarkers.dt(df, variables, value='count', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA)))
  dt <- mapMarkers.dt(df, variables, value='count', evilMode = 'allVariables')
  cols <- unlist(lapply(as.list(variables), function(x) {veupathUtils::getColName(x@variableSpec)}))
  expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, cols, with=FALSE])))


  # Numeric x
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'geo'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  overlayBins <- veupathUtils::getDiscretizedBins(df$entity.contA[complete.cases(df$entity.contA)])

  dt <- mapMarkers.dt(df, variables, value='count', xValues = overlayBins)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- mapMarkers.dt(df, variables, value='count', xValues = overlayBins, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA)))
  # TODO box cant have evilMode = 'allVariables' bc we cant calculate bins with NA
  # dt <- mapMarkers.dt(df, variables, value='count', evilMode = 'allVariables')
  # expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))
})

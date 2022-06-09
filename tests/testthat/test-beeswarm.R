context('beeswarm')

test_that("beeswarm.dt() returns a valid plot.data beeswarm object", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, map, 0.2, TRUE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'beeswarm')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'beeswarm')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(dt$entity.cat3[[1]], 'cat3_a')
  expect_equal(dt$label[[1]], c('cat4_a','cat4_b','cat4_c','cat4_d'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(42,42,29,51))
})

test_that("beeswarm.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.cat5'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, map, 0.2, TRUE)
  expect_equal(class(dt$median[[1]]), 'numeric')
  
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat5)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')


  #single group
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- testDF[testDF$entity.cat3 == 'cat3_a' & testDF$entity.cat4 == 'cat4_a',]

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  expect_equal(class(dt$median[[1]]), 'numeric')

  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("beeswarm.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- as.data.frame(testDF)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))


  map <- data.frame('id' = c('entity.contB', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label',  'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  map <- data.frame('id' = c('entity.int7', 'entity.contA', 'entity.int6'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.2, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  
  dt <- beeswarm.dt(df, map, 0.2, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  

  # With factors
  df <- testDF
  map <- data.frame('id' = c('entity.factor3', 'entity.contB', 'entity.cat4'),
                  'plotRef' = c('facetVariable1', 'yAxisVariable', 'xAxisVariable'),
                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.factor3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(class(dt$entity.factor3), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  map <- data.frame('id' = c('entity.factor3','entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable2', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),9)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(class(dt$panel), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  map <- data.frame('id' = c('entity.factor3','entity.factor6', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable2', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(class(dt$panel), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  
  ## Collection vars
  # Multiple vars to x
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'xAxisVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(unique(dt$label)[[1]], c('contA','contB','contC'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  
  # Use displayLabels
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'),
                    'displayLabel' = c('Y','X','Z'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, TRUE, collectionVariablePlotRef = 'xAxisVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(unique(dt$label)[[1]], c('X','Y','Z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')

  
  # Multiple vars to facet1
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$entity.facetVariable1, c('contA','contB','contC'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  computedVariableMetadata = list('displayName' = c('VarLabel1','VarLabel2'),
                                  'displayRangeMin' = '0',
                                  'displayRangeMax' = '1',
                                  'collectionVariable' = list('collectionType' = 'abundance'))

  dt <- beeswarm.dt(df, map, 0.1, TRUE, collectionVariablePlotRef = 'facetVariable1', computedVariableMetadata = computedVariableMetadata)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$panel[1], 'contA.||.cat4_a')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel', 'naToZero'))
  expect_equal(names(attr(dt, 'computedVariableMetadata')), c('displayName','displayRangeMin','displayRangeMax','collectionVariable'))
  expect_equal(attr(dt, 'computedVariableMetadata')$displayName, computedVariableMetadata$displayName)
  expect_equal(attr(dt, 'computedVariableMetadata')$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(attr(dt, 'computedVariableMetadata')$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(attr(dt, 'computedVariableMetadata')$collectionVariable$collectionType, computedVariableMetadata$collectionVariable$collectionType)


  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'facetVariable2')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$panel[1], 'cat4_a.||.contA')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'facetVariable2')$variableId, 'facetVariable2')
  expect_equal(names(attr(dt, 'facetVariable1')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel', 'naToZero'))
  
  
  # Handle only one var sent as a collectionVar
  map <- data.frame('id' = c('entity.contB','entity.cat3'),
                    'plotRef' = c('xAxisVariable','overlayVariable'),
                    'dataType' = c('NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'xAxisVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(unique(dt$label)[[1]], c('contB'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  
  
  map <- data.frame('id' = c('entity.contB','entity.cat3'),
                    'plotRef' = c('facetVariable1','xAxisVariable'),
                    'dataType' = c('NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$entity.facetVariable1, c('contB'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
})

test_that("beeswarm() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(jsonList$beeswarm$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('groupLabel','yLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, TRUE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues', 'median'))
  expect_equal(jsonList$beeswarm$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$beeswarm$config$xVariableDetails$variableId, 'cat4')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('groupLabel','yLabel','panelLabel'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('','','panelLabel'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$beeswarm$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.binA'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.contB', 'entity.binA'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.2, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('label','rawData', 'jitteredValues'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  

  # w forceStringType we shouldnt see categorical numbers any more
  #map <- data.frame('id' = c('entity.int7', 'entity.contA', 'entity.int6'),
  #                  'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
  #                  'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
  #                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  #dt <- beeswarm.dt(df, map, 0.1, FALSE)
  #outJson <- getJSON(dt, FALSE)
  #jsonList <- jsonlite::fromJSON(outJson)
  #expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  #expect_equal(names(jsonList$beeswarm), c('data','config'))
  #expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails', 'label', 'rawData', 'jitteredValues'))
  #expect_equal(jsonList$beeswarm$data$overlayVariableDetails$variableId[[1]], 'int7')
  #expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  #expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  #expect_equal(jsonList$beeswarm$config$xVariableDetails$variableId, 'int6')
  #expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  #expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  #expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'int6')
  #expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  #expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  #expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  #expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'contA', 'int7'))
  #expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'contA', 'int7'))
  #expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
  
  # Multiple vars for x and computed variable metadata
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  computedVariableMetadata = list('displayName' = c('VarLabel1','VarLabel2'),
                                  'displayRangeMin' = '0.5',
                                  'displayRangeMax' = '1.5',
                                  'collectionVariable' = list('collectionType' = 'abundance'))
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'xAxisVariable', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata), c('displayName', 'displayRangeMin', 'displayRangeMax','collectionVariable'))
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayName, computedVariableMetadata$displayName)
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 2)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
  # Multiple vars to facet1
  map <- data.frame('id' = c('entity.contB', 'entity.contA','entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  computedVariableMetadata = list('displayName' = c('VarLabel1','VarLabel2'),
                                  'collectionVariable' = list('collectionType' = 'abundance'))
  
  
  dt <- beeswarm.dt(df, map, 0.1, TRUE, collectionVariablePlotRef = 'facetVariable1', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues', 'median'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata), c('displayName','collectionVariable'))
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayName, computedVariableMetadata$displayName)
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 2)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  # Multiple vars to facet2
  map <- data.frame('id' = c('entity.contB', 'entity.contA','entity.cat3','entity.cat4'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  computedVariableMetadata = list('displayRangeMin' = '2002-08-28 EST',
                                  'displayRangeMax' = '2002-09-28 EST',
                                  'collectionVariable' = list('collectionType' = 'abundance'))
  
  dt <- beeswarm.dt(df, map, 0.2, FALSE, collectionVariablePlotRef = 'facetVariable2', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata), c('displayRangeMin', 'displayRangeMax','collectionVariable'))
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(jsonList$beeswarm$config$computedVariableMetadata$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$beeswarm$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 2)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 4)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')


  # With continuous overlay variable (< 9 values)
  map <- data.frame('id' = c('entity.int6', 'entity.contB', 'entity.binA'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$beeswarm$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
})


test_that("beeswarm.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4', 'entity.cat5'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- beeswarm.dt(df, map, 0.1, TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB) & !is.na(df$entity.cat4))) 
  #dt <- beeswarm.dt(df, map, 0.1, TRUE, evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))


  ## Using naToZero to change some NAs to 0
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4', 'entity.cat5'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'),
                    'naToZero' = c('FALSE', TRUE, '', NA), stringsAsFactors=FALSE)


  dt <- beeswarm.dt(df, map, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contB' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contB', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- beeswarm.dt(df, map, 0.1, TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.cat4)))


  ## Collection vars
  # Multiple vars to x

  # Add nMissing missing values to each column -- TODO address that setting na to zero above changes df
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'xAxisVariable')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases))
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing)
  expect_true(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1])
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df))


  # Multiple vars to facet1
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- beeswarm.dt(df, map, 0.1, FALSE, collectionVariablePlotRef = 'facetVariable1')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases))
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1])
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)
})


test_that("beeswarm.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  map_string <- data.frame('id' = c('entity.contA', 'entity.cat7', 'entity.cat5'),
                           'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'),
                           'dataType' = c('NUMBER', 'STRING', 'STRING'),
                           'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_string <- beeswarm.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.contA', 'entity.int7', 'entity.cat5'),
                        'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'),
                        'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                        'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_num <- beeswarm.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
  map_string <- data.frame('id' = c('entity.contA', 'entity.cat7', 'entity.cat5'),
                           'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                           'dataType' = c('NUMBER', 'STRING', 'STRING'),
                           'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_string <- beeswarm.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.contA', 'entity.int7', 'entity.cat5'),
                        'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                        'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                        'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
                        
  dt_num <- beeswarm.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
})

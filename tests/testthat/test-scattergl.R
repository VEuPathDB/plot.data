context('scattergl')

test_that("scatter.dt does not fail when there are no complete cases.", {
  map <- data.frame('id' = c('entity.int', 'entity.cont'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS'),
                    stringsAsFactors = FALSE)
  df <- data.noneComplete[is.na(entity.int),]

  dt <- scattergl.dt(df, map, 'raw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, map, value='smoothedMeanWithRaw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, map, value='bestFitLineWithRaw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, map, value='density')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$densityX), TRUE)
  expect_equal(is.list(dt$densityY), TRUE)

  map <- data.frame('id' = c('entity.cont', 'entity.int', 'entity.binary2'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'),
                    stringsAsFactors = FALSE)
  df <- data.noneComplete

  dt <- scattergl.dt(df, map, 'raw')
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)
})

test_that("scattergl.dt() returns a valid plot.data scatter object", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.dateA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'scatterplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'facetVariable1'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 12)
})

test_that("scattergl.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.dateA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')


  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
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

test_that("scattergl.dt() returns an appropriately sized data.table", {
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.dateA', 'entity.cat4', ''),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING', ''),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$entity.dateA)), range(dt$seriesX))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.dateA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$entity.dateA)), range(dt$seriesX))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))


  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  numericSeriesX <- lapply(dt$seriesX, as.numeric)
  expect_equal(range(df$entity.contA), range(numericSeriesX))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'densityX', 'densityY'))

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
 
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
 
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'densityX', 'densityY'))


  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'densityX', 'densityY'))
  

  map <- data.frame('id' = c('entity.contB', 'entity.contA'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
    
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('densityX', 'densityY'))

  map <- data.frame('id' = c('entity.contC', 'entity.contB', 'entity.contA'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_true(identical(dt$seriesGradientColorscale[[1]], as.character(df$entity.contC)))
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('entity.cat4', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))

  ## Collection vars
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('contA','contB','contC'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('Y','X','Z','',''), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'X.||.cat3_a')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel', 'naToZero'))
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable2')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'cat3_a.||.contA')
  expect_equal(attr(dt, 'facetVariable2')$variableId, 'facetVariable2')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(names(attr(dt, 'facetVariable1')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel', 'naToZero'))
  
  # With computed var
  computedVariableMetadata = list('displayName' = 'Pielou\'s Evenness',
                                  'displayRangeMin' = '0',
                                  'displayRangeMax' = '1',
                                  'collectionVariable' = list('collectionType' = 'abundance'))
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('Y','X','Z','',''), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'overlayVariable', computedVariableMetadata = computedVariableMetadata)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('X','Y','Z'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(names(attr(dt, 'computedVariableMetadata')), c('displayName','displayRangeMin','displayRangeMax','collectionVariable'))
  expect_equal(attr(dt, 'computedVariableMetadata')$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(attr(dt, 'computedVariableMetadata')$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(attr(dt, 'computedVariableMetadata')$displayName, computedVariableMetadata$displayName)
  expect_equal(attr(dt, 'computedVariableMetadata')$collectionVariable$collectionType, computedVariableMetadata$collectionVariable$collectionType)
  
  # Only one var in the collectionVar
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat3'),
                    'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'overlayVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('contB'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.cat3', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('contB'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  
  # With factors
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.factor3'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.factor3','seriesX','seriesY'))
  expect_equal(class(dt$entity.factor3), 'character')
  
  map <- data.frame('id' = c('entity.factor6', 'entity.contB', 'entity.contA', 'entity.factor3'),
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 18)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.factor3'),
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')
})

test_that("scattergl() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$xVariableDetails$variableId, 'contA')
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'contB', 'cat3', 'cat4'))
  

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4', ''),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING', ''),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$xVariableDetails$variableId, 'contA')
  expect_equal(jsonList$scatterplot$config$yVariableDetails$variableId, 'contB')
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId')) 
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','cat3','cat4'))
  
  # Continuous overlay
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$overlayVariableDetails$variableId, 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))


  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), 'displayLabel' = c('yDisplay', 'xDisplay', 'panelDisplay', 'zDisplay'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$scatterplot$config$overlayVariableDetails$variableId, 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), 'displayLabel' = c('', 'xDisplay', '', 'zDisplay'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$scatterplot$config$overlayVariableDetails$variableId, 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))

  # With collection vars and computed variable metadata
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  computedVariableMetadata = list('displayName' = c('VarLabel1','VarLabel2'),
                                  'displayRangeMin' = '0',
                                  'displayRangeMax' = '1',
                                  'collectionVariable' = list('collectionType' = 'abundance'))

  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable1', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata), c('displayName','displayRangeMin','displayRangeMax','collectionVariable'))
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayName, computedVariableMetadata$displayName)
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 3)
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)

  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  computedVariableMetadata = list('displayRangeMin' = '0.5',
                                  'displayRangeMax' = '1.5',
                                  'collectionVariable' = list('collectionType' ='abundance'))

  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'facetVariable2', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata), c('displayRangeMin','displayRangeMax','collectionVariable'))
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayRangeMin, computedVariableMetadata$displayRangeMin)
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayRangeMax, computedVariableMetadata$displayRangeMax)
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 3)
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.contD', 'entity.cat3'),
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  computedVariableMetadata = list('displayName' = c('VarLabel1','VarLabel2'),
                                  'collectionVariable' = list('collectionType' = 'abundance'))

  dt <- scattergl.dt(df, map, 'raw', collectionVariablePlotRef = 'overlayVariable', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','computedVariableMetadata','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata), c('displayName','collectionVariable'))
  expect_equal(jsonList$scatterplot$config$computedVariableMetadata$displayName, computedVariableMetadata$displayName)
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable), c('collectionType','collectionVariablePlotRef','collectionValuePlotRef','collectionVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$scatterplot$config$computedVariableMetadata$collectionVariable$collectionVariableDetails), 3)
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
})



test_that("scattergl.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  # Add nMissing missing values to each column
  set.seed(123)
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  dt <- scattergl.dt(df, map, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- scattergl.dt(df, map, value = 'raw', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA) & !is.na(df$entity.contB)))
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], length(unlist(dt$seriesX)))
  #dt <- scattergl.dt(df, map, value = 'raw', evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))

  ## Using naToZero to change some NAs to 0
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    'naToZero' = c(FALSE,'', 'TRUE', NA), stringsAsFactors=FALSE)


  dt <- scattergl.dt(df, map, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contA' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contA', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- scattergl.dt(df, map, value = 'raw', evilMode='strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB)))
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], length(unlist(dt$seriesX)))

  # SeriesGradientColorscale with no data
  map <- data.frame('id' = c('entity.contC', 'entity.contB', 'entity.contA'),
                  'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                  'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw', evilMode = 'strataVariables')
  expect_equal(nrow(dt), 2)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(lapply(dt$seriesGradientColorscale, length), lapply(dt$seriesX, length))
  expect_true(all(is.na(dt$seriesGradientColorscale[[2]])))

  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat4', 'entity.contC'),
                  'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'),
                  'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'),
                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)

  dt <- scattergl.dt(df, map, 'raw', evilMode = 'strataVariables')
  expect_equal(names(dt), c('entity.cat4', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(lapply(dt$seriesGradientColorscale, length), lapply(dt$seriesX, length))
  expect_true(all(is.na(dt$seriesGradientColorscale[[2]])))

  # Testing json output with continuous overlay and no data
  outJson <- getJSON(dt, TRUE)
  jsonList <- jsonlite::fromJSON(outJson) # Turns all nulls in arrays to NA!!
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 10)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_true(all(is.na(jsonList$scatterplot$data$seriesGradientColorscale[[2]]))) # should be null in the json file, but fromJSON converts to NA
  expect_equal(names(jsonList$scatterplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$overlayVariableDetails$variableId, 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))

})

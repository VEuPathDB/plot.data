context('scattergl')

test_that("scattergl.dt() returns a valid plot.data scatter object", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'scatterplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','overlayVariable', 'facetVariable1'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.group','entity.panel','size'))
  expect_equal(nrow(sampleSizes), 16)
})

test_that("scattergl.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(class(unlist(dt$entity.panel)), 'character')
  expect_equal(class(unlist(dt$entity.group)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')


  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(class(unlist(dt$entity.panel)), 'character')
  expect_equal(class(unlist(dt$entity.group)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("scattergl.dt() returns an appropriately sized data.table", {
  
  map <- data.frame('id' = c('entityA.group', 'entityB.contVar', 'entityC.date', 'entityD.panel', ''), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING', ''), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), stringsAsFactors=FALSE)
  df <- data.dates
  names(df) <- c('entityA.group','entityD.panel','entityC.date','entityB.contVar')
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityA.group', 'entityD.panel', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$entityC.date)), range(dt$seriesX))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityA.group', 'entityD.panel', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityD.panel', 'entityA.group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityD.panel', 'entityA.group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$entity.date)), range(dt$seriesX))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.panel', 'entity.group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.panel', 'entity.group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))


  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'seriesX', 'seriesY'))
  numericSeriesX <- lapply(dt$seriesX, as.numeric)
  expect_equal(range(df$entity.x), range(numericSeriesX))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.panel', 'entity.group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.panel', 'entity.group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'densityX', 'densityY'))

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
 
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
 
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'densityX', 'densityY'))


  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'densityX', 'densityY'))
  

  map <- data.frame('id' = c('entity.y', 'entity.x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

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
  
  
  # Cases with repeated vars for one plot element
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  df$entity.z <- df$entity.x + df$entity.y
  df$entity.w <- df$entity.x - df$entity.y
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('entity.group', 'meltedVariable', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$meltedVariable), c('entity.x', 'entity.y', 'entity.z'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'meltedVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('meltedVariable', 'entity.group', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$meltedVariable), c('entity.x', 'entity.y', 'entity.z'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'meltedVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')

  # Continuous overlay
  map <- data.frame('id' = c('entity.z', 'entity.y', 'entity.x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- data.xy
  df$entity.z <- runif(500)
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_true(identical(dt$seriesGradientColorscale[[1]], df$entity.z))
  
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel', 'entity.z'), 'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel', 'entity.z'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('entity.panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))
  
  
})

test_that("scattergl() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 16)
  expect_equal(names(jsonList$scatterplot$config),c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId')) 
  

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel', ''), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING', ''), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 16)
  expect_equal(names(jsonList$scatterplot$config),c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId')) 
  

  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel', 'entity.z'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- data.xy
  df$entity.z <- runif(500)
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$scatterplot$config),c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel', 'entity.z'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), 'displayLabel' = c('yDisplay', 'xDisplay', 'panelDisplay', 'zDisplay'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$scatterplot$config),c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel', 'entity.z'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), 'displayLabel' = c('', 'xDisplay', '', 'zDisplay'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$scatterplot$config),c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails','overlayVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$overlayVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
})




test_that("scattergl.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  # Add 10 missing values to each column
  df$entity.x[sample(1:100, 10, replace=F)] <- NA
  df$entity.y[sample(1:100, 10, replace=F)] <- NA
  df$entity.group[sample(1:100, 10, replace=F)] <- NA
  df$entity.panel[sample(1:100, 10, replace=F)] <- NA
  
  dt <- scattergl.dt(df, map, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCases')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], 0)
  dt <- scattergl.dt(df, map, value = 'raw', evilMode=TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], sum((is.na(df$entity.group) | is.na(df$entity.panel)) & !is.na(df$entity.x) & !is.na(df$entity.y)))
  expect_equal(sum(attr(dt, 'plottedIncompleteCases')[1], attr(dt, 'completeCases')[1]), length(unlist(dt$seriesX)))
})

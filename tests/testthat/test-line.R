context('lineplot')

test_that("lineplot.dt() returns a valid plot.data lineplot object", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.repeatedDateA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'lineplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'facetVariable1'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 12)
})

test_that("lineplot.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.repeatedDateA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, map, 'median')
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


  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)


  dt <- lineplot.dt(df, map, 'mean')
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

test_that("lineplot.dt() returns an appropriately sized data.table", {

  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.repeatedDateA', 'entity.cat4', ''),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'),
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING', ''),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.cat4'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  map <- data.frame('id' = c('entity.contB', 'entity.repeatedContA', 'entity.cat4'), 
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors = FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  map <- data.frame('id' = c('entity.contB', 'entity.repeatedContA'), 
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))


  ## List vars
  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('contB','contC','contD'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('Y','X','Z','',''), stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'facetVariable1',)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'X.||.cat3_a')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'facetVariable2')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'cat3_a.||.contB')
  expect_equal(attr(dt, 'facetVariable2')$variableId, 'facetVariable2')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(names(attr(dt, 'facetVariable1')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    'displayLabel' = c('Y','X','Z','',''), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'median', collectionVarPlotRef = 'overlayVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('X','Y','Z'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  # Only one var in the collectionVar
  map <- data.frame('id' = c('entity.contB', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER','STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)

  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'overlayVariable')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('contB'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  
  map <- data.frame('id' = c('entity.contB', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER','STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)

  dt <- lineplot.dt(df, map, 'median', collectionVarPlotRef = 'facetVariable1')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.cat3', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('contB'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')


  # With factors
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.factor3'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'mean')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.factor3','seriesX','seriesY'))
  expect_equal(class(dt$entity.factor3), 'character')
  
  map <- data.frame('id' = c('entity.factor6', 'entity.contB', 'entity.repeatedContA', 'entity.factor3'), 
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'median')
  expect_equal(nrow(dt), 18)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.factor3'), 
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'mean')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')
})

test_that("lineplot() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.cat4'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, map, 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$xVariableDetails$variableId, 'repeatedContA')
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedContA', 'contB', 'cat3', 'cat4'))

  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'facetVariable1')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','collectionVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$collectionVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)

  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'facetVariable2')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','collectionVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$collectionVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contC', 'entity.contD', 'entity.repeatedContA', 'entity.cat3'), 
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', collectionVarPlotRef = 'overlayVariable')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','collectionVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$collectionVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
})

test_that("lineplot.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.repeatedContA', 'entity.cat4'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  dt <- lineplot.dt(df, map, 'mean')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- lineplot.dt(df, map, value = 'mean', evilMode=TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.repeatedContA) & !is.na(df$entity.contB)))
})

context('lineplot')

test_that("lineplot.dt() returns a valid plot.data lineplot object", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.dates, data.dates, data.dates)
  df$entity.contVar <- rnorm(3000)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'lineplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'facetVariable1'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.group','entity.panel','size'))
  expect_equal(nrow(sampleSizes), 16)
})

test_that("lineplot.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.dates, data.dates, data.dates)
  df$entity.contVar <- rnorm(3000)

  dt <- lineplot.dt(df, map, 'median')
  expect_equal(class(unlist(dt$entity.panel)), 'character')
  expect_equal(class(unlist(dt$entity.group)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')


  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.xy, data.xy, data.xy)
  df$entity.y <- rnorm(1500)

  dt <- lineplot.dt(df, map, 'mean')
  expect_equal(class(unlist(dt$entity.panel)), 'character')
  expect_equal(class(unlist(dt$entity.group)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("lineplot.dt() returns an appropriately sized data.table", {

  map <- data.frame('id' = c('entityA.group', 'entityB.contVar', 'entityC.date', 'entityD.panel', ''), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'), 
                    'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING', ''), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', ''), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.dates, data.dates, data.dates)
  df$entity.contVar <- rnorm(3000)
  names(df) <- c('entityA.group','entityD.panel','entityC.date','entityB.contVar')

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityA.group', 'entityD.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entityA.group', 'entityD.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 
                             'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                             'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                             'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                             stringsAsFactors=FALSE)
  df <- rbind(data.xy, data.xy, data.xy)
  df$entity.y <- rnorm(1500)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.panel'), 
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors = FALSE)

  dt <- lineplot.dt(df, map, 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, map, 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.panel', 'seriesX', 'seriesY'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  map <- data.frame('id' = c('entity.y', 'entity.x'), 
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

  # With factors
  df$entity.factor1 <- factor(sample(c('mon','tues','wed','thurs','fri'), size = nrow(df), replace = T))
  df$entity.factor2 <- factor(sample(c('red','orange','yellow'), size = nrow(df), replace = T))
  
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.factor1'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'mean')
  expect_equal(nrow(dt), 20)
  expect_equal(names(dt), c('entity.group', 'entity.factor1','seriesX','seriesY'))
  expect_equal(class(dt$entity.factor1), 'character')
  
  map <- data.frame('id' = c('entity.factor2', 'entity.y', 'entity.x', 'entity.factor1'), 
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'median')
  expect_equal(nrow(dt), 15)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.factor1'), 
                    'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'mean')
  expect_equal(nrow(dt), 20)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')
})

test_that("lineplot() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.xy, data.xy, data.xy)
  df$entity.y <- rnorm(1500)
  dt <- lineplot.dt(df, map, 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 16)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'panel')
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$xVariableDetails$variableId, 'x')
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('x', 'y', 'group', 'panel'))

  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df$entity.z <- df$entity.x + df$entity.y
  df$entity.w <- df$entity.x - df$entity.y
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','listVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$listVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)

  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'facetVariable2', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','listVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$listVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
  
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'overlayVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$lineplot$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails','listVariableDetails'))
  expect_equal(names(jsonList$lineplot$config$listVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
})


test_that("lineplot.dt() handles list vars as overlay and facet", {

  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.xy, data.xy, data.xy)
  df$entity.y <- rnorm(1500)
  df$entity.z <- df$entity.x + df$entity.y
  df$entity.w <- df$entity.x - df$entity.y
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('entity.group', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('x','y','z'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('Y','X','Z','',''), stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'X.||.group1')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'facetVariable2', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'group1.||.x')
  expect_equal(attr(dt, 'facetVariable2')$variableId, 'facetVariable2')
  expect_equal(attr(dt, 'facetVariable2')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(names(attr(dt, 'facetVariable1')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.w', 'entity.group'), 
                    'plotRef' = c('overlayVariable', 'overlayVariable', 'overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    'displayLabel' = c('Y','X','Z','',''), 
                    stringsAsFactors=FALSE)
  
  dt <- lineplot.dt(df, map, 'median', listVarPlotRef = 'overlayVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.group', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('X','Y','Z'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'overlayVariable')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  
  # Only one var in the listVar
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.group'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'NUMBER','STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'mean', listVarPlotRef = 'overlayVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('entity.overlayVariable', 'entity.group', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.overlayVariable), c('y'))
  expect_equal(attr(dt, 'overlayVariable')$variableId, 'overlayVariable')
  expect_equal(attr(dt, 'overlayVariable')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.group'), 
                    'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 
                    'dataType' = c('NUMBER', 'NUMBER','STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  dt <- lineplot.dt(df, map, 'median', listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('entity.group', 'entity.facetVariable1', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.facetVariable1), c('y'))
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
})


test_that("lineplot.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x', 'entity.panel'), 
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- rbind(data.xy, data.xy, data.xy)
  df$entity.y <- rnorm(1500)
  
  # Add 10 missing values to each column
  df$entity.x[sample(1:100, 10, replace=F)] <- NA
  df$entity.y[sample(1:100, 10, replace=F)] <- NA
  df$entity.group[sample(1:100, 10, replace=F)] <- NA
  df$entity.panel[sample(1:100, 10, replace=F)] <- NA
  
  dt <- lineplot.dt(df, map, 'mean')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- lineplot.dt(df, map, value = 'mean', evilMode=TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.x) & !is.na(df$entity.y)))
})

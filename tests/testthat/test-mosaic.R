context('mosaic')

test_that("mosaic.dt() returns a valid plot.data mosaic object", {
  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(test.df)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','facetVariable1', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.binA','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('oddsratio','relativerisk','orInterval','rrInterval','pvalue','entity.cat4'))

  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map, statistic='chiSq')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','facetVariable1', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.binA','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chisq','pvalue', 'degreesFreedom','entity.cat4'))

  map <- data.frame('id' = c('entity.cat3', 'entity.cat7', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','facetVariable1', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.cat7','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chisq','pvalue', 'degreesFreedom','entity.cat4'))
})

test_that("mosaic.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(test.df)

  dt <- mosaic.dt(df, map)
  expect_equal(class(unlist(dt$xLabel)), 'character')
  expect_equal(class(unlist(dt$yLabel)), 'character')
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(unlist(namedAttrList$statsTable$oddsratio)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$relativerisk)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$orInterval)), c('scalar', 'character'))
  expect_equal(class(unlist(namedAttrList$statsTable$rrInterval)), c('scalar', 'character'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$entity.cat4)), 'character')

  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map, statistic = 'chiSq')
  expect_equal(class(unlist(dt$xLabel)), 'character')
  expect_equal(class(unlist(dt$yLabel)), 'character')
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(unlist(namedAttrList$statsTable$chisq)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$degreesFreedom)), c('scalar', 'integer'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$entity.cat4)), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.cat7', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  expect_equal(class(unlist(dt$xLabel)), 'character')
  expect_equal(class(unlist(dt$yLabel)), 'character')
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(unlist(namedAttrList$statsTable$chisq)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$degreesFreedom)), c('scalar', 'integer'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$entity.cat4)), 'character')
})

test_that("mosaic.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(test.df)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.cat4'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('oddsratio', 'relativerisk', 'orInterval', 'rrInterval', 'pvalue', 'entity.cat4')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat4','entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  map <- data.frame('id' = c('entity.binB', 'entity.binA'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('oddsratio', 'relativerisk', 'orInterval', 'rrInterval', 'pvalue')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map, statistic = 'chiSq')
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.cat4'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom', 'entity.cat4')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat4','entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  map <- data.frame('id' = c('entity.binB', 'entity.binA'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map, statistic = 'chiSq')
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.cat7', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.cat4'))
  expect_equal(dt$xLabel[[1]],paste0("cat7_", letters[1:7]))
  expect_equal(dt$yLabel[[1]][[1]],paste0("cat3_", letters[1:3]))
  expect_equal(length(dt$value[[1]]),7)
  expect_equal(length(dt$value[[1]][[1]]),3)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom', 'entity.cat4')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat4','entity.cat7','size'))
  expect_equal(class(sampleSizeTable$entity.cat7[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.cat7'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  expect_equal(dt$xLabel[[1]],paste0("cat7_", letters[1:7]))
  expect_equal(dt$yLabel[[1]][[1]],paste0("cat3_", letters[1:3]))
  expect_equal(length(dt$value[[1]]),7)
  expect_equal(length(dt$value[[1]][[1]]),3)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat7','size'))
  expect_equal(class(sampleSizeTable$entity.cat7[[1]]), 'character')
  
  
  # df <- as.data.frame(data.numcat)
  # map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1', 'entity.strcat1'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  # dt <- mosaic.dt(df, map)
  # expect_is(dt, 'data.table')
  # expect_is(dt$value, 'list')
  # expect_is(dt$value[[1]], 'list')
  # expect_equal(nrow(dt),3)
  # expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.strcat1'))
  # expect_equal(dt$xLabel[[1]],c('1','2','3'))
  # expect_equal(dt$yLabel[[1]][[1]],c('1','2','3','4','5'))
  # expect_equal(length(dt$value[[1]]),3)
  # expect_equal(length(dt$value[[1]][[1]]),5)
  # statsTable <- statsTable(dt)
  # expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom', 'entity.strcat1')))
  # sampleSizeTable <- sampleSizeTable(dt)
  # expect_equal(names(sampleSizeTable),c('entity.strcat1','entity.numcat1','size'))
  # expect_equal(class(sampleSizeTable$entity.numcat1[[1]]), 'character')


  # With factors
  # map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1', 'entity.factor1'),
  #                   'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
  #                   'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
  #                   'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
                    
  # dt <- mosaic.dt(df, map)
  # expect_is(dt, 'data.table')
  # expect_is(dt$value, 'list')
  # expect_is(dt$value[[1]], 'list')
  # expect_equal(nrow(dt),5)
  # expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.factor1'))
  # expect_equal(class(dt$entity.factor1), 'character')

  # map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1', 'entity.factor1', 'entity.factor2'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  # dt <- mosaic.dt(df, map)
  # expect_is(dt, 'data.table')
  # expect_is(dt$value, 'list')
  # expect_is(dt$value[[1]], 'list')
  # expect_equal(nrow(dt),15)
  # expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'cat4'))

  # map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1', 'entity.strcat1', 'entity.factor2'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  # dt <- mosaic.dt(df, map)
  # expect_is(dt, 'data.table')
  # expect_is(dt$value, 'list')
  # expect_is(dt$value[[1]], 'list')
  # expect_equal(nrow(dt),9)
  # expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'cat4'))
  
})

test_that("mosaic() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(test.df)

  dt <- mosaic.dt(df, map)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$mosaic$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$mosaic$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$xVariableDetails$variableId, 'binA')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'binA')
  expect_equal(names(jsonList$statsTable),c('oddsratio','relativerisk','orInterval','rrInterval','pvalue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('binA', 'binB', 'cat4'))

  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'),
                    'displayLabel' = c('groupLabel','varLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$mosaic$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$mosaic$config$xVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$mosaic$config$xVariableDetails$variableId, 'binA')
  expect_equal(names(jsonList$mosaic$config$yVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'binA')
  expect_equal(names(jsonList$statsTable),c('oddsratio','relativerisk','orInterval','rrInterval','pvalue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('binA', 'binB', 'cat4'))

  map <- data.frame('id' = c('entity.binB', 'entity.binA', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'),
                    'displayLabel' = c('','varLabel',''), stringsAsFactors=FALSE)

  dt <- mosaic.dt(df, map)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$mosaic$config$xVariableDetails),c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$mosaic$config$yVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  # df <- data.numcat
  # map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  # dt <- mosaic.dt(df, map)
  # outJson <- getJSON(dt, FALSE)
  # jsonList <- jsonlite::fromJSON(outJson)
  
  # expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  # expect_equal(names(jsonList$mosaic),c('data','config'))
  # expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value'))
  # expect_equal(names(jsonList$mosaic$config),c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  # expect_equal(names(jsonList$mosaic$config$xVariableDetails),c('variableId','entityId'))
  # expect_equal(jsonList$mosaic$config$xVariableDetails$variableId, 'numcat1')
  # expect_equal(names(jsonList$sampleSizeTable),c('xVariableDetails','size'))
  # expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  # expect_equal(names(jsonList$statsTable),c('chisq','pvalue','degreesFreedom'))
  # expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  # expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  
})


# test_that("mosaic.dt() returns same shaped outputs for string cats and num cats.", {
  
#   df <- data.numcat
  
#   map_string <- data.frame('id' = c('entity.strcat1', 'entity.strcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#   dt_string <- mosaic.dt(df, map_string)
  
#   map_num <- data.frame('id' = c('entity.numcat1', 'entity.numcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#   dt_num <- mosaic.dt(df, map_num)
  
#   expect_equal(nrow(dt_string), nrow(dt_num))
#   expect_equal(names(dt_string), names(dt_num))
#   expect_equal(length(dt_string$xLabel[[1]]), length(dt_num$xLabel[[1]]))
#   expect_equal(length(dt_string$yLabel[[1]]), length(dt_num$yLabel[[1]]))
#   expect_equal(length(dt_string$value[[1]]), length(dt_num$value[[1]]))
#   expect_equal(dt_string$entity.cat4, dt_num$entity.cat4)
  
# })


# test_that("mosaic.dt() returns correct information about missing data", {
#   map <- data.frame('id' = c('entity.cat3', 'entity.binA', 'entity.cat4'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
#   df <- data.binary
  
#   # Add 10 missing values to each column
#   df$entity.binA[sample(1:100, 10, replace=F)] <- NA
#   df$entity.cat3[sample(1:100, 10, replace=F)] <- NA
#   df$entity.cat4[sample(1:100, 10, replace=F)] <- NA
  
#   dt <- mosaic.dt(df, map)
#   expect_equal(names(statsTable(dt)),c('oddsratio', 'relativerisk', 'orInterval', 'rrInterval', 'pvalue', 'entity.cat4'))
#   completecasestable <- completeCasesTable(dt)
#   # Each entry should equal NROW(df) - 10
#   expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
#   # number of completeCases should be <= complete cases for each var
#   expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
#   expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
#   dt <- mosaic.dt(df, map, evilMode = TRUE)
#   expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.cat3) & !is.na(df$entity.binA)))
# })

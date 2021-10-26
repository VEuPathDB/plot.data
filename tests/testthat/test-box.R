context('box')

test_that("box.dt() returns a valid plot.data box object", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(names(namedAttrList$statsTable), c('entity.cat4','statistic','pvalue','parameter','method','statsError'))
  
  dt <- box.dt(df, map, 'all', FALSE, computeStats = T)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(names(namedAttrList$statsTable), c('entity.cat4','statistic','pvalue','parameter','method','statsError'))
  expect_equal(dt$entity.cat3[[1]], 'cat3_a')
  expect_equal(dt$label[[1]], c('cat4_a','cat4_b','cat4_c','cat4_d'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(42,42,29,51))
})

test_that("box.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.cat5'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(testDF)

  dt <- box.dt(df, map, 'none', TRUE)
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')
  
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat5)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  
  #w outliers
  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')
  #first group has no outliers, want json like [] rather than {}
  expect_equal(class(dt$outliers[[1]][[1]]), 'list')
  expect_equal(class(dt$outliers[[1]][[3]]), 'numeric')
  
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

  dt <- box.dt(df, map, 'none', TRUE)
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')

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

test_that("box.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- as.data.frame(testDF)
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))


  map <- data.frame('id' = c('entity.contB', 'entity.cat4'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label',  'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))

  map <- data.frame('id' = c('entity.int7', 'entity.contA', 'entity.int6'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  
  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  
  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  
  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  
  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))
  
  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  
  # Test single outlier
  df <- data.frame('entity.x' = c('group1'), 'entity.y'=c(35.1, 34.2, 36.2, 90.2))
  map <- data.frame('id' = c('entity.y', 'entity.x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'outliers', FALSE, computeStats = T)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$outliers[[1]]), 'list')
  
  # With factors
  df <- testDF
  map <- data.frame('id' = c('entity.factor3', 'entity.contB', 'entity.cat4'),
                  'plotRef' = c('facetVariable1', 'yAxisVariable', 'xAxisVariable'),
                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.factor3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$entity.factor3), 'character')

  map <- data.frame('id' = c('entity.factor3','entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable2', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),9)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.factor3','entity.factor6', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable2', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$panel), 'character')
  
  ## List vars
  # Multiple vars to x
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('contA','contB','contC'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$displayLabel, 'listVarName')
  
  # Use displayLabels
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'),
                    'displayLabel' = c('Y','X','Z'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('X','Y','Z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$displayLabel, 'listVarName')

  
  # Multiple vars to facet1
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.facetVariable1, c('contA','contB','contC'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  
  
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$panel[1], 'contA.||.cat4_a')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))

  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.contC', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable2', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$panel[1], 'cat4_a.||.contA')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'facetVariable2')$variableId, 'facetVariable2')
  expect_equal(attr(dt, 'facetVariable2')$displayLabel, 'listVarName')
  expect_equal(names(attr(dt, 'facetVariable1')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  
  # Handle only one var sent as a listVar
  map <- data.frame('id' = c('entity.contB','entity.cat3'),
                    'plotRef' = c('xAxisVariable','overlayVariable'),
                    'dataType' = c('NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('contB'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$displayLabel, 'listVarName')
  
  
  map <- data.frame('id' = c('entity.contB','entity.cat3'),
                    'plotRef' = c('facetVariable1','xAxisVariable'),
                    'dataType' = c('NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.facetVariable1, c('contB'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
})

test_that("box() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- as.data.frame(testDF)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'cat4')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('groupLabel','yLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$boxplot$config$xVariableDetails$variableId, 'cat4')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'cat4')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('facetVariable1', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('groupLabel','yLabel','panelLabel'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$facetVariableDetails[[1]]$variableId, 'cat3')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('','','panelLabel'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.binA'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'binA')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  map <- data.frame('id' = c('entity.contB', 'entity.binA'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$statsTable$statistic), 'integer')
  expect_equal(length(jsonList$statsTable$statistic), 1)
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  
  map <- data.frame('id' = c('entity.contB', 'entity.cat3'),
                    'plotRef' = c('yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'STRING'),
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$statsTable), c('statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(length(jsonList$statsTable$statistic), 1)
  expect_equal(class(jsonList$statsTable$statsError), 'character')

  map <- data.frame('id' = c('entity.int7', 'entity.contA', 'entity.int6'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[[1]], 'int7')
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$boxplot$config$xVariableDetails$variableId, 'int6')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'int6')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'contA', 'int7'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'int6')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'contA', 'int7'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  
  # Multiple vars for x
  map <- data.frame('id' = c('entity.contB', 'entity.contA', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'xAxisVariable', 'overlayVariable'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails', 'listVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$listVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  # Multiple vars to facet1
  map <- data.frame('id' = c('entity.contB', 'entity.contA','entity.cat3'),
                    'plotRef' = c('facetVariable1', 'facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats=T, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails', 'listVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$listVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  # Multiple vars to facet2
  map <- data.frame('id' = c('entity.contB', 'entity.contA','entity.cat3','entity.cat4'),
                    'plotRef' = c('facetVariable2', 'facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER', 'NUMBER','STRING','STRING'),
                    'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats=T, listVarPlotRef = 'facetVariable2', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails','yVariableDetails', 'listVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$listVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
})


test_that("box.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- box.dt(df, map, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- box.dt(df, map, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB) & !is.na(df$entity.cat4))) 
})


test_that("box.dt() returns an appropriately sized statistics table", {
  map <- data.frame('id' = c('entity.cat6', 'entity.contB'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  
  df <- as.data.frame(testDF)
  
  ## Kruskal-Wallis
  # No overlay, no facets
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 5)
  expect_equal(names(statsTable), c('statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method[[1]]), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError[[1]]), c('scalar', 'character'))
  
  # No overlay, one facet
  map <- data.frame('id' = c('entity.cat6', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay, no facets
  map <- data.frame('id' = c('entity.cat6', 'entity.contB', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat6))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat6', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay and facet
  map <- data.frame('id' = c('entity.cat6', 'entity.contB', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat6)*uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 7)
  expect_equal(names(statsTable), c('entity.cat6', 'entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  ## Wilcoxon
  map <- data.frame('id' = c('entity.binA', 'entity.contB'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)

  # No overlay, no facets
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 5)
  expect_equal(names(statsTable), c('statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method[[1]]), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError[[1]]), c('scalar', 'character'))
  
  # No overlay, one facet
  map <- data.frame('id' = c('entity.binA', 'entity.contB', 'entity.cat4'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay, no facets
  map <- data.frame('id' = c('entity.binA', 'entity.contB', 'entity.cat3'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.binA))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.binA', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay and facet
  map <- data.frame('id' = c('entity.binA', 'entity.contB', 'entity.cat3', 'entity.cat4'),
                    'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.binA)*uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 7)
  expect_equal(names(statsTable), c('entity.binA', 'entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
})



test_that("box.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  map_string <- data.frame('id' = c('entity.contA', 'entity.cat7', 'entity.cat5'),
                           'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'),
                           'dataType' = c('NUMBER', 'STRING', 'STRING'),
                           'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_string <- box.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.contA', 'entity.int7', 'entity.cat5'),
                        'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'),
                        'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                        'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_num <- box.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
  map_string <- data.frame('id' = c('entity.contA', 'entity.cat7', 'entity.cat5'),
                           'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                           'dataType' = c('NUMBER', 'STRING', 'STRING'),
                           'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt_string <- box.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.contA', 'entity.int7', 'entity.cat5'),
                        'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'),
                        'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                        'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
                        
  dt_num <- box.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
})
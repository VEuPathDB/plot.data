context('box')

test_that("box.dt() returns a valid plot.data box object", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.table::as.data.table(data.xy)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.group','entity.panel','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('entity.panel','statistics'))
  
  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.group','entity.panel','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('entity.panel','statistics'))
  expect_equal(dt$entity.group[[1]], 'group1')
  expect_equal(dt$label[[1]], c('panel1','panel2','panel3','panel4'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(25,50,25,25))
})

test_that("box.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

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
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("box.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))


  map <- data.frame('id' = c('entity.y', 'entity.panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

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

  
  df <- as.data.frame(data.numcat)
  map <- data.frame('id' = c('entity.numcat2', 'entity.cont1', 'entity.numcat1'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  
  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  
  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  
  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  
  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))
  
  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),5)
  expect_equal(names(dt),c('entity.numcat2', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  
})

test_that("box.dt() accepts listVars for both x axis and facet vars", {  
  ## Case when we input multiple vars as one to x axis
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.group'), 'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- data.xy
  df[, entity.z := entity.x+entity.y]
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('entity.x','entity.y','entity.z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'listValueName')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$displayLabel, 'listVarName')

  
  ## Case when we input multiple vars as one to facet 1
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.facetVariable1, c('entity.x','entity.y','entity.z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'listValueName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.group', 'entity.panel'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable', 'facetVariable2'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'inferredVarName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$panel[1], 'entity.x.||.panel1')
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'inferredVarName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
  expect_equal(names(attr(dt, 'facetVariable2')), c('variableId', 'entityId', 'dataType', 'dataShape', 'displayLabel'))
  
  
  # Handle only one var sent as a listVar
  map <- data.frame('id' = c('entity.y','entity.group'), 'plotRef' = c('xAxisVariable','overlayVariable'), 'dataType' = c('NUMBER','STRING'), 'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt),c('entity.group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('entity.y'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'listValueName')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'xAxisVariable')
  expect_equal(attr(dt, 'xAxisVariable')$displayLabel, 'listVarName')
  
  # Handle only one var sent as a listVar
  map <- data.frame('id' = c('entity.y','entity.group'), 'plotRef' = c('facetVariable1','xAxisVariable'), 'dataType' = c('NUMBER','STRING'), 'dataShape' = c('CONTINUOUS','CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('entity.facetVariable1', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.facetVariable1, c('entity.y'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'yAxisVariable')
  expect_equal(attr(dt, 'yAxisVariable')$displayLabel, 'listValueName')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'facetVariable1')
  expect_equal(attr(dt, 'facetVariable1')$displayLabel, 'listVarName')
})

test_that("box() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('entity.panel','statistics','overlayVariableDetails'))

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('groupLabel','yLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$statsTable), c('entity.panel','statistics','overlayVariableDetails'))

  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('','','panelLabel'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  
  df <- as.data.frame(data.numcat)
  map <- data.frame('id' = c('entity.numcat2', 'entity.cont1', 'entity.numcat1'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('entity.numcat1','statistics','overlayVariableDetails'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  
  ## Case when we input multiple vars as one to x axis
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.group'), 'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  df <- data.xy
  df[, entity.z := entity.x+entity.y]
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'xAxisVariable', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails', 'listVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$listVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('entity.xAxisVariable','statistics','overlayVariableDetails'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  ## Input multiple vars as one to facet 1
  map <- data.frame('id' = c('entity.y', 'entity.x', 'entity.z', 'entity.group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, listVarPlotRef = 'facetVariable1', listVarDisplayLabel = 'listVarName', inferredVarDisplayLabel = 'listValueName')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails', 'listVariableDetails'))
  expect_equal(names(jsonList$boxplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$config$yVariableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(names(jsonList$boxplot$config$listVariableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistics'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
})



test_that("box.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- data.numcat
  
  map_string <- data.frame('id' = c('entity.cont1', 'entity.strcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_string <- box.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.cont1', 'entity.numcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_num <- box.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
  map_string <- data.frame('id' = c('entity.cont1', 'entity.strcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_string <- box.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.cont1', 'entity.numcat2', 'entity.myoverlay'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_num <- box.dt(df, map_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
})


test_that("box.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  # Add 10 missing values to each column
  df$entity.x[sample(1:100, 10, replace=F)] <- NA
  df$entity.y[sample(1:100, 10, replace=F)] <- NA
  df$entity.group[sample(1:100, 10, replace=F)] <- NA
  df$entity.panel[sample(1:100, 10, replace=F)] <- NA
  dt <- box.dt(df, map, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCases')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], 0)
  dt <- box.dt(df, map, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], sum(is.na(df$entity.group) & !is.na(df$entity.y) & !is.na(df$entity.panel))) 
})

test_that("box.dt() returns an appropriately sized statistics table", {
  map <- data.frame('id' = c('entity.xcat', 'entity.y'), 'plotRef' = c('xAxisVariable', 'yAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.xy)
  df$entity.xcat <- sample(c('x1','x2','x3'), 500, replace=T) # Add another categorical var
  
  # No overlay, no facets
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 1)
  
  # No overlay, one facet
  map <- data.frame('id' = c('entity.xcat', 'entity.y', 'entity.panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$entity.panel))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay, no facets
  map <- data.frame('id' = c('entity.xcat', 'entity.y', 'entity.group'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$entity.xcat))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay and facet
  map <- data.frame('id' = c('entity.xcat', 'entity.y', 'entity.group', 'entity.panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$entity.xcat)*uniqueN(df$entity.panel))
  expect_equal(ncol(statsTable), 3)
  
  
})

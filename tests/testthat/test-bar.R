context('bar')

test_that("bar.dt does not fail when there are no complete cases.", {
  map <- data.frame('id' = c('entity.binary1'),
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('STRING'),
                    'dataShape' = c('CATEGORICAL'),
                    stringsAsFactors = FALSE)
  df <- data.noneComplete[is.na(entity.binary1),]

  dt <- bar.dt(df, map, value='count')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)

  dt <- bar.dt(df, map, value='proportion')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)

  map <- data.frame('id' = c('entity.binary1', 'entity.binary2'),
                    'plotRef' = c('xAxisVariable', 'overlayVariable'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'),
                    stringsAsFactors = FALSE)
  df <- data.noneComplete

  dt <- bar.dt(df, map, value='count')
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
})

test_that("bar.dt() returns a valid plot.data barplot object", {
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'barplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','facetVariable1', 'facetVariable2'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','entity.x','size'))
  expect_equal(nrow(sampleSizes), 16)
})

test_that("bar.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
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
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("bar.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts should sum to 1 for each panel. Checking panel1.||.group2
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$panel == 'panel1.||.group2']$label), function(v, dt) {dt[dt$panel=='panel1.||.group2']$value[[v]][which(dt[dt$panel == 'panel1.||.group2']$label[[v]] == df$entity.x[1])]}, dt))),1)
  

  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('entity.group', 'entity.panel', 'label', 'value'))
  # sum of x counts should sum to 1 for each panel. Checking panel 1
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$entity.panel == 'panel1']$label), function(v, dt) {dt[dt$entity.panel=='panel1']$value[[v]][which(dt[dt$entity.panel == 'panel1']$label[[v]] == df$entity.x[1])]}, dt))),1)
  

  map <- data.frame('id' = c('entity.group', 'entity.x'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.group', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(sum(unlist(lapply(seq_along(dt$label), function(v, dt) {dt$value[[v]][which(dt$label[[v]] == df$entity.x[1])]}, dt))),1)

  map <- data.frame('id' = c('entity.x'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('STRING'), 'dataShape' = c('CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  
  map <- data.frame('id' = c('entity.strcat1', 'entity.numcat1', 'entity.numcat2'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.numcat)
  
  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),15)
  expect_equal(names(dt),c('entity.strcat1', 'entity.numcat2', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),15)
  expect_equal(names(dt),c('entity.strcat1', 'entity.numcat2', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),15)
  expect_equal(names(dt),c('entity.strcat1', 'entity.numcat2', 'label', 'value'))
  # sum of x counts should sum to 1 for each panel. Checking panel 1
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$entity.numcat2 == '1']$label), function(v, dt) {dt[dt$entity.numcat2=='1']$value[[v]][which(dt[dt$entity.numcat2 == '1']$label[[v]] == df$entity.numcat1[1])]}, dt))),1)

  # With factors
  df <- as.data.frame(data.binned)
  df$entity.factor1 <- factor(sample(c('mon','tues','wed','thurs','fri'), size = nrow(df), replace = T))
  df$entity.factor2 <- factor(sample(c('red','orange','yellow'), size = nrow(df), replace = T))
  
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.factor1'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.group', 'entity.factor1', 'label', 'value'))
  expect_equal(class(dt$entity.factor1), 'character')

  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.factor1'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.factor2', 'entity.x', 'entity.factor1'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),15)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  
})

test_that("bar() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$xVariableDetails$variableId, 'x')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'x')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('x', 'group', 'panel'))

  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), 'displayLabel' = c('groupLabel','xLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$barplot$data$overlayVariableDetails$variableId[1], 'group')
  expect_equal(names(jsonList$barplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$barplot$config$xVariableDetails$variableId, 'x')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'panel')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'x')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('x', 'group', 'panel'))
  

  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), 'displayLabel' = c('groupLabel','','panelLabel'), stringsAsFactors=FALSE)
  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$data$facetVariableDetails[[1]]), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$barplot$data$facetVariableDetails[[1]]$variableId, 'panel')
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$xVariableDetails$variableId, 'x')
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'group')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  
  df <- as.data.frame(data.numcat)
  map <- data.frame('id' = c('entity.numcat2', 'entity.numcat1', 'entity.strcat1'), 'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('completeCasesAllVars','completeCasesAxesVars','xVariableDetails'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$xVariableDetails$variableId, 'numcat1')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'strcat1')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('numcat1', 'strcat1', 'numcat2'))

})


test_that("bar.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- data.numcat
  
  map_string <- data.frame('id' = c('entity.strcat1', 'entity.strcat2', 'entity.myoverlay'), 'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_string <- bar.dt(df, map_string)
  
  map_num <- data.frame('id' = c('entity.numcat1', 'entity.numcat2', 'entity.myoverlay'), 'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_num <- bar.dt(df, map_num)
  
  expect_equal(dim(dt_string), dim(dt_num))
  expect_equal(names(dt_string), c('entity.myoverlay', 'entity.strcat1', 'label', 'value'))
  expect_equal(names(dt_num), c('entity.myoverlay', 'entity.numcat1', 'label', 'value'))
  expect_equal(dt_string$entity.myoverlay, dt_num$entity.myoverlay)
  expect_equal(length(dt_string$label[[1]]), length(dt_num$label[[1]]))
  expect_equal(length(dt_string$value[[1]]), length(dt_num$value[[1]]))
  
})

test_that("bar.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.group', 'entity.x', 'entity.panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)
  
  # Add 10 missing values to each column
  df$entity.x[sample(1:100, 10, replace=F)] <- NA
  df$entity.y[sample(1:100, 10, replace=F)] <- NA
  df$entity.group[sample(1:100, 10, replace=F)] <- NA
  df$entity.panel[sample(1:100, 10, replace=F)] <- NA
  dt <- bar.dt(df, map, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- bar.dt(df, map, value='count', evilMode = TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.x)))
})

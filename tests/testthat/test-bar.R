context('bar')

test_that("bar.dt() returns a valid plot.data barplot object", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'barplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','facetVariable1', 'facetVariable2'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','x','size'))
  expect_equal(nrow(sampleSizes), 16)
})

test_that("bar.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt$label, 'list')
  expect_equal(class(unlist(dt$label)), 'character')
  expect_is(dt$value, 'list')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("bar.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
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
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$panel == 'panel1.||.group2']$label), function(v, dt) {dt[dt$panel=='panel1.||.group2']$value[[v]][which(dt[dt$panel == 'panel1.||.group2']$label[[v]] == df$x[1])]}, dt))),1)
  

  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))
  # sum of x counts should sum to 1 for each panel. Checking panel 1
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$panel == 'panel1']$label), function(v, dt) {dt[dt$panel=='panel1']$value[[v]][which(dt[dt$panel == 'panel1']$label[[v]] == df$x[1])]}, dt))),1)
  

  map <- data.frame('id' = c('group', 'x'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, map, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(sum(unlist(lapply(seq_along(dt$label), function(v, dt) {dt$value[[v]][which(dt$label[[v]] == df$x[1])]}, dt))),1)

  map <- data.frame('id' = c('x'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('STRING'), 'dataShape' = c('CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
})

test_that("bar() returns appropriately formatted json", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))

  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), 'displayLabel' = c('groupLabel','xLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))

  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), 'displayLabel' = c('groupLabel','','panelLabel'), stringsAsFactors=FALSE)
  dt <- bar.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$data$facetVariableDetails[[1]]), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
})


test_that("bar.dt() returns same shaped outputs for string cats and num cats.", {
  
  npoints <- 500
  df <- data.frame('strcat1' = sample(c('cat1','cat2','cat3'), npoints, T),
                   'strcat2' = sample(c('color1','color2','color3','color4','color5'), npoints, T),
                   'numcat1' = sample(1:3, npoints, T),
                   'numcat2' = sample(1:5, npoints, T),
                   'myoverlay' = sample(c('group1','group2'), npoints, T))
  
  map_string <- data.frame('id' = c('strcat1', 'strcat2', 'myoverlay'), 'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_string <- bar.dt(df, map_string)
  
  map_num <- data.frame('id' = c('numcat1', 'numcat2', 'myoverlay'), 'plotRef' = c('facetVariable1', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt_num <- bar.dt(df, map_num)
  
  expect_equal(dim(dt_string), dim(dt_num))
  expect_equal(names(dt_string), c('myoverlay', 'strcat1', 'label', 'value'))
  expect_equal(names(dt_num), c('myoverlay', 'numcat1', 'label', 'value'))
  expect_equal(dt_string$myoverlay, dt_num$myoverlay)
  expect_equal(length(dt_string$label[[1]]), length(dt_num$label[[1]]))
  expect_equal(length(dt_string$value[[1]]), length(dt_num$value[[1]]))
  
})

test_that("bar.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)
  
  # Add 10 missing values to each column
  df$x[sample(1:100, 10, replace=F)] <- NA
  df$y[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  dt <- bar.dt(df, map, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCases')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], 0)
  dt <- bar.dt(df, map, value='count', evilMode = TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], sum((is.na(df$group) | is.na(df$panel)) & !is.na(df$x))) 
})

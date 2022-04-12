context('pie')

test_that("pie.dt does not fail when there are no complete cases.", {
  map <- data.frame('id' = c('entity.binary1'),
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('STRING'),
                    'dataShape' = c('CATEGORICAL'),
                    stringsAsFactors = FALSE)
  df <- data.noneComplete[is.na(entity.binary1),]

  dt <- pie.dt(df, map, value='count')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- pie.dt(df, map, value='proportion')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)
})

test_that("pie.dt() returns a valid plot.data pieplot object", {
  map <- data.frame('id' = c('entity.cat3', 'entity.cat6', 'entity.cat4'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- pie.dt(df, map, value='count')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'pieplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','facetVariable1', 'facetVariable2', 'rankedValues', 'viewport'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','entity.cat6','size'))
  expect_equal(nrow(sampleSizes), 12)
})


test_that("pie.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.cat4', 'entity.cat6', 'entity.cat5'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(testDF)

  dt <- pie.dt(df, map, value='count')
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

test_that("pie.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('entity.cat3', 'entity.cat6', 'entity.cat4'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(testDF)

  dt <- pie.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'pieplot')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- pie.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_is(dt, 'pieplot')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)

  map <- data.frame('id' = c('entity.cat6'), 
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('STRING'),
                    'dataShape' = c('CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- pie.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  dt <- pie.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  # With factors
  map <- data.frame('id' = c('entity.cat4','entity.cat6', 'entity.factor6'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- pie.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'pieplot')
  expect_equal(nrow(dt),24)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  map <- data.frame('id' = c('entity.factor3', 'entity.cat6', 'entity.factor6'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- pie.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'pieplot')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  
})

test_that("pie() returns appropriately formatted json", {
  map <- data.frame('id' = c('entity.int11', 'entity.cat5', 'entity.cont5', 'entity.cont6'),
                    'plotRef' = c('xAxisVariable', 'facetVariable1', 'latitudeVariable', 'longitudeVariable'),
                    'dataType' = c('STRING', 'STRING', 'NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CONTINUOUS', 'LONGITUDE'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)
  df$entity.int11 <- as.character(df$entity.int11)
  viewport <- list('latitude'=list('xMin'=-.5,
                                   'xMax'=.5),
                   'longitude'=list('left'=-.5,
                                    'right'=.5))

  dt <- pie.dt(df, map, value='count', viewport=viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot), c('data','config'))
  expect_equal(names(jsonList$pieplot$data), c('facetVariableDetails','label','value'))
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','latitudeVariable','longitudeVariable','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('5','3','9','8','10','2','6','Other'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$pieplot$config$viewport)), 'numeric')
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'int11')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'int11')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int11', 'cat5'))

  map <- data.frame('id' = c('entity.int11', 'entity.cat5'),
                    'plotRef' = c('xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)
  df$entity.int11 <- as.character(df$entity.int11)

  dt <- pie.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot), c('data','config'))
  expect_equal(names(jsonList$pieplot$data), c('facetVariableDetails','label','value'))
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('5','3','9','8','10','2','6','Other'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(class(unlist(jsonList$pieplot$config$viewport)), 'NULL')
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'int11')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'int11')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int11', 'cat5'))

  map <- data.frame('id' = c('entity.cat6', 'entity.cat5'),
                    'plotRef' = c('xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  df <- as.data.frame(testDF)

  dt <- pie.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot), c('data','config'))
  expect_equal(names(jsonList$pieplot$data), c('facetVariableDetails','label','value'))
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('cat6_e','cat6_b','cat6_d','cat6_a','cat6_c','cat6_f'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'cat6')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat5'))

  map <- data.frame('id' = c('entity.cat6', 'entity.cat5'),
                    'plotRef' = c('xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'),
                    'displayLabel' = c('xLabel','panelLabel'), stringsAsFactors=FALSE)

  dt <- pie.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot), c('data','config'))
  expect_equal(names(jsonList$pieplot$data), c('facetVariableDetails','label','value'))
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('cat6_e','cat6_b','cat6_d','cat6_a','cat6_c','cat6_f'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'cat6')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat5')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat5'))
  

  map <- data.frame('id' = c('entity.cat6', 'entity.cat5'),
                    'plotRef' = c('xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'),
                    'displayLabel' = c('','panelLabel'), stringsAsFactors=FALSE)

  dt <- pie.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot$data$facetVariableDetails[[1]]), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$pieplot$data$facetVariableDetails[[1]]$variableId, 'cat5')
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('cat6_e','cat6_b','cat6_d','cat6_a','cat6_c','cat6_f'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'cat6')
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  
  map <- data.frame('id' = c('entity.int7', 'entity.int6'),
                    'plotRef' = c('facetVariable1', 'xAxisVariable'),
                    'dataType' = c('NUMBER', 'NUMBER'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  
  dt <- pie.dt(df, map, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('pieplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$pieplot), c('data','config'))
  expect_equal(names(jsonList$pieplot$data), c('facetVariableDetails','label','value'))
  expect_equal(names(jsonList$pieplot$config), c('completeCasesAllVars','completeCasesAxesVars','rankedValues', 'viewport', 'xVariableDetails'))
  expect_equal(jsonList$pieplot$config$rankedValues, c('6','3','1','4','2','5'))
  expect_equal(class(jsonList$pieplot$config$rankedValues), 'character')
  expect_equal(names(jsonList$pieplot$config$xVariableDetails), c('variableId','entityId'))
  expect_equal(jsonList$pieplot$config$xVariableDetails$variableId, 'int6')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'int7'))

})



test_that("pie.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('entity.cat3', 'entity.contA', 'entity.cat4'),
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'),
                    'dataType' = c('STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- pie.dt(df, map, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- pie.dt(df, map, value='count', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA)))
  dt <- pie.dt(df, map, value='count', evilMode = 'allVariables')
  expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))
})

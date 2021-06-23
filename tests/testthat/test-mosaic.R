context('mosaic')

test_that("mosaic.dt() returns a valid plot.data mosaic object", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','completeCasesTable','sampleSizeTable','facetVariable1', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','var','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('oddsratio','relativerisk','orInterval','rrInterval','pvalue','panel'))
})

test_that("mosaic.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_equal(class(unlist(dt$xLabel)), 'character')
  expect_equal(class(unlist(dt$yLabel)), 'character')
  expect_equal(class(unlist(dt$panel)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(unlist(namedAttrList$statsTable$oddsratio)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$relativerisk)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$orInterval)), c('scalar', 'character'))
  expect_equal(class(unlist(namedAttrList$statsTable$rrInterval)), c('scalar', 'character'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$panel)), 'character')
})

test_that("mosaic.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'panel'))
  expect_equal(dt$xLabel[[1]],c('var1','var2'))
  expect_equal(dt$yLabel[[1]],c('group1','group2'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('oddsratio', 'relativerisk', 'orInterval', 'rrInterval', 'pvalue', 'panel')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('panel','var','size'))
  expect_equal(class(sampleSizeTable$var[[1]]), 'character')

  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt, 'mosaic')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('oddsratio', 'relativerisk', 'orInterval', 'rrInterval', 'pvalue')))
})

test_that("mosaic() returns appropriately formatted json", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- mosaic.dt(data.binary, map)
  outJson <- getJSON(dt)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$mosaic$config),c('completeCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$mosaic$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$statsTable),c('oddsratio','relativerisk','orInterval','rrInterval','pvalue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
})


test_that("mosaic.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.binary
  
  # Add 10 missing values to each column
  df$var[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  
  dt <- mosaic.dt(df, map)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCases')[1] <= completecasestable$completeCases), TRUE) 
})

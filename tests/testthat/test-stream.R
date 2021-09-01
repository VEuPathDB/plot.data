context('stream')

test_that("stream.dt() returns a valid plot.data stream object", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- stream.dt(df, map)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'stream')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','overlayVariable', 'facetVariable1'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.group','entity.panel','size'))
  expect_equal(nrow(sampleSizes), 16)
})

test_that("stream.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('entity.group', 'entity.contVar', 'entity.date', 'entity.panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- stream.dt(df, map, 'raw')
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


  map <- data.frame('id' = c('entity.group', 'entity.y', 'entity.x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)

  dt <- stream.dt(data.xy, map)
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



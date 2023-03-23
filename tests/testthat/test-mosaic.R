context('mosaic')

test_that("mosaic.dt() does not fail when 2x2 version only has 1 value on an axis.", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- testDF

  dt <- mosaic.dt(df, variables, 'all')
  expect_equal(dt$xLabel[[1]], 'cat1_a')
  expect_equal(dt$yLabel[[1]][[1]], c('cat2_a', 'cat2_b'))
  expect_equal(dt$value[[1]][[1]], c(251, 249))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- testDF

  dt <- mosaic.dt(df, variables, 'all')
  expect_equal(dt$xLabel[[1]], c('cat2_a', 'cat2_b'))
  expect_equal(dt$yLabel[[1]][[1]], 'cat1_a')
  expect_equal(dt$value[[1]][[1]], 251)
})

test_that("mosaic.dt() does not fail when there are no complete cases.", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- data.noneComplete

  dt <- mosaic.dt(df, variables)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.na(attr$statsTable$oddsRatio@value), TRUE)
  expect_equal(is.list(dt$xLabel), TRUE)
  expect_equal(is.list(dt$yLabel), TRUE)
  expect_equal(is.list(dt$yLabel[[1]]), TRUE)
  expect_equal(is.list(dt$value), TRUE)
  expect_equal(is.list(dt$value[[1]]), TRUE)
})

test_that("mosaic.dt() returns a valid plot.data mosaic object", {
 
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.binA','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chiSq','fisher','oddsRatio','relativeRisk','prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue','entity.cat4'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables, statistic='chiSq')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.binA','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chisq','pvalue', 'degreesFreedom','entity.cat4'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.cat7','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chisq','pvalue', 'degreesFreedom','entity.cat4'))


  # Testing an RxC with 'all' statistics
  dt <- mosaic.dt(df, variables, statistic = 'all')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat4','entity.cat7','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('chiSq','entity.cat4'))


  # Ensure sampleSizeTable and completeCasesTable do not get returned if we do not ask for them.
  dt <- mosaic.dt(df, variables, sampleSizes = FALSE, completeCases = FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'mosaic')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables','statsTable'))
  expect_equal(names(namedAttrList$statsTable), c('chisq','pvalue', 'degreesFreedom','entity.cat4'))
})

test_that("mosaic.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mosaic.dt(df, variables)
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

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables, statistic = 'chiSq')
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
  expect_equal(class(unlist(namedAttrList$statsTable$degreesFreedom)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$entity.cat4)), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
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
  expect_equal(class(unlist(namedAttrList$statsTable$degreesFreedom)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$pvalue)), c('scalar', 'numeric'))
  expect_equal(class(unlist(namedAttrList$statsTable$entity.cat4)), 'character')

})

test_that("mosaic.dt() returns an appropriately sized data.table", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.cat4'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- attributes(dt)$statsTable
  expect_equal(names(statsTable), c(c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue','entity.cat4')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat4','entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  expect_equal(dt$xLabel[[1]],c('binA_a','binA_b'))
  expect_equal(dt$yLabel[[1]][[1]],c('binB_a','binB_b'))
  expect_equal(length(dt$value[[1]]),2)
  expect_equal(length(dt$value[[1]][[1]]),2)
  statsTable <- attributes(dt)$statsTable
  expect_equal(names(statsTable), c(c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.binA','size'))
  expect_equal(class(sampleSizeTable$entity.binA[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables, statistic = 'chiSq')
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

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables, statistic = 'chiSq')
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

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
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

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
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

  # With 'all' statistics for an RxC
  dt <- mosaic.dt(df, variables, statistic = 'all')
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
  expect_equal(names(statsTable), c(c('chiSq')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.cat7','size'))
  expect_equal(class(sampleSizeTable$entity.cat7[[1]]), 'character')


  # With factors
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value'))
  expect_equal(dt$xLabel[[1]],paste0("factor6_", letters[1:6]))
  expect_equal(dt$yLabel[[1]][[1]],paste0("factor3_", letters[1:3]))
  expect_equal(length(dt$value[[1]]),6)
  expect_equal(length(dt$value[[1]][[1]]),3)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.factor6','size'))
  expect_equal(class(sampleSizeTable$entity.factor6[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'entity.factor3'))
  expect_equal(dt$xLabel[[1]],paste0("cat7_", letters[1:7]))
  expect_equal(dt$yLabel[[1]][[1]],paste0("cat3_", letters[1:3]))
  expect_equal(length(dt$value[[1]]),7)
  expect_equal(length(dt$value[[1]][[1]]),3)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom', 'entity.factor3')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('entity.factor3','entity.cat7','size'))
  expect_equal(class(sampleSizeTable$entity.cat7[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  expect_is(dt, 'data.table')
  expect_is(dt$value, 'list')
  expect_is(dt$value[[1]], 'list')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('xLabel', 'yLabel', 'value', 'panel'))
  expect_equal(dt$xLabel[[1]],as.character(1:6))
  expect_equal(dt$yLabel[[1]][[1]],paste0("cat3_", letters[1:3]))
  expect_equal(length(dt$value[[1]]),6)
  expect_equal(length(dt$value[[1]][[1]]),3)
  statsTable <- statsTable(dt)
  expect_equal(names(statsTable), c(c('chisq', 'pvalue', 'degreesFreedom', 'panel')))
  sampleSizeTable <- sampleSizeTable(dt)
  expect_equal(names(sampleSizeTable),c('panel','entity.int6','size'))
  expect_equal(class(sampleSizeTable$entity.int6[[1]]), 'character')
})

test_that("mosaic() returns appropriately formatted json", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- mosaic.dt(testDF, variables)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 1)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat1')
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('cat1','binB','binA'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'binA')
  expect_equal(names(jsonList$statsTable),c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('binA', 'binB', 'cat1'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- mosaic.dt(df, variables)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('cat4','binB','binA'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'binA')
  expect_equal(names(jsonList$statsTable),c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('binA', 'binB', 'cat4'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "varLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('cat4','binB','binA'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'binA')
  expect_equal(names(jsonList$statsTable),c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue','facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('binA', 'binB', 'cat4'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "varLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 4)
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId', 'displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- mosaic.dt(df, variables)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value'))
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('int6','int7'))
  expect_equal(names(jsonList$sampleSizeTable),c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$statsTable),c('chisq','pvalue','degreesFreedom'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))


  # Ensure sampleSizeTable and completeCasesTable are not part of json if we do not ask for them.
  dt <- mosaic.dt(df, variables, sampleSizes = FALSE, completeCases = FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('mosaic','statsTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value'))
  expect_equal(names(jsonList$mosaic$config),c('variables'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('int6','int7'))
  expect_equal(names(jsonList$statsTable),c('chisq','pvalue','degreesFreedom'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- mosaic.dt(df, variables, evilMode = 'strataVariables')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value'))
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('int6','int7'))
  expect_equal(names(jsonList$sampleSizeTable),c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId')) 


  # Check json structure of stats table when statistics = 'all' for rxc
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- mosaic.dt(df, variables, statistic = 'all')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('mosaic','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$mosaic),c('data','config'))
  expect_equal(names(jsonList$mosaic$data),c('xLabel','yLabel','value','facetVariableDetails'))
  expect_equal(names(jsonList$mosaic$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$mosaic$data$facetVariableDetails), 3)
  expect_equal(jsonList$mosaic$data$facetVariableDetails[[1]]$variableId, 'cat3')
  expect_equal(names(jsonList$mosaic$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$mosaic$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$mosaic$config$variables$variableSpec$variableId, c('int6','int7','cat3'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'int7')
  expect_equal(names(jsonList$statsTable),c('chiSq', 'facetVariableDetails'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int7', 'int6', 'cat3'))

})

test_that("mosaic.dt() returns correct information about missing data", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  dt <- mosaic.dt(df, variables)
  expect_equal(names(attributes(dt)$statsTable),c('chiSq', 'fisher', 'oddsRatio', 'relativeRisk', 'prevalence','sensitivity','specificity','posPredictiveValue','negPredictiveValue', 'entity.cat4'))
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- mosaic.dt(df, variables, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.binB) & !is.na(df$entity.binA)))
  dt <- mosaic.dt(df, variables, evilMode = 'allVariables')
  cols <- unlist(lapply(as.list(variables), function(x) {veupathUtils::getColName(x@variableSpec)}))
  expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, cols, with=FALSE])))
})

test_that("mosaic.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt_string <- mosaic.dt(df, variables_string)
  
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))  

  dt_num <- mosaic.dt(df, variables_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(length(dt_string$xLabel[[1]]), length(dt_num$xLabel[[1]]))
  expect_equal(length(dt_string$yLabel[[1]]), length(dt_num$yLabel[[1]]))
  expect_equal(length(dt_string$value[[1]]), length(dt_num$value[[1]]))
  expect_equal(dt_string$entity.cat4, dt_num$entity.cat4)
  
})



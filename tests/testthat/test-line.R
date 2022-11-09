context('lineplot')

test_that("lineplot.dt() does not fail when there are no complete cases.", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- data.noneComplete[is.na(entity.int),]

  dt <- lineplot.dt(df, variables, binWidth = NULL, value = 'mean', viewport = NULL)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$errorBars), TRUE)
  expect_equal(is.list(dt$errorBars[[1]]), TRUE)
  expect_equal(is.list(dt$binSampleSize), TRUE)
  expect_equal(is.list(dt$binSampleSize[[1]]), TRUE)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)
})

test_that("lineplot.dt() returns a valid plot.data lineplot object", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, binWidth = NULL, value = 'mean', viewport = NULL)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'lineplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables',
                                      'completeCasesAllVars',
                                      'completeCasesAxesVars',
                                      'completeCasesTable',
                                      'sampleSizeTable',
                                      'viewport',
                                      'binSlider',
                                      'binSpec'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 12)
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
})

test_that("lineplot.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'proportion', binWidth = NULL, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
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
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'proportion', binWidth = NULL, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  expect_true(!any(grepl('T00:00:00', unlist(dt$binStart))))
  expect_true(!any(grepl('T00:00:00', unlist(dt$binEnd))))
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  #0:0 proportions should be NA entries in a numeric list
  expect_equal(all(unlist(dt$binSampleSize[[12]][8]) == 0), TRUE)
  expect_equal(is.na(dt$seriesY[[12]][8]), TRUE)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'median', binWidth = NULL)
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  expect_true(!any(grepl('T00:00:00', unlist(dt$binStart))))
  expect_true(!any(grepl('T00:00:00', unlist(dt$binEnd))))
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, NULL, 'mean', NULL)
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contPositive', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df$entity.contPositive <- rnorm(500, 10)

  dt <- lineplot.dt(df, variables, NULL, 'geometricMean', NULL)
  expect_equal(class(unlist(dt$entity.cat4)), 'character')
  expect_equal(class(unlist(dt$entity.cat3)), 'character')
  expect_equal(class(unlist(dt$seriesX)), 'character')
  expect_equal(class(unlist(dt$seriesY)), 'character')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
})

test_that("lineplot.dt() returns an appropriately sized data.table", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, NULL, 'mean', TRUE, NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, variables, NULL, 'median', TRUE, NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, NULL, 'proportion', TRUE, NULL, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))

  dt <- lineplot.dt(df, variables, NULL, 'proportion', TRUE, NULL, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contPositive', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df$entity.contPositive <- rnorm(500, 10)

  dt <- lineplot.dt(df, variables, value = 'geometricMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  ## no error bars
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean', errorBars=F)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  dt <- lineplot.dt(df, variables, value = 'median', errorBars=F)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'proportion', errorBars=F, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'binSampleSize', 'binStart', 'binEnd'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))

  ## Collection vars
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.collection', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(unique(dt$entity.collection), c('contB','contC','contD'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(dt$panel[1], 'contB.||.cat3_a')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(dt$panel[1], 'cat3_a.||.contB')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet2')@variableId, 'collection')
  
  # With computed var
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = 'Pielou\'s Evenness',
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.collection', 'entity.cat3', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(unique(dt$entity.collection), c('contB','contC','contD'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'overlay')@variableId, 'collection')
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[2]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[2]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[2]]@displayRangeMax)

  # Only one var in the collectionVar
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.collection', 'entity.cat3', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(unique(dt$entity.collection), c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'overlay')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.cat3', 'entity.collection', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(unique(dt$entity.collection), c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')

  # With factors
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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.factor3','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(class(dt$entity.factor3), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'median')
  expect_equal(nrow(dt), 18)
  expect_equal(names(dt), c('panel','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(class(dt$panel), 'character')

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
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(class(dt$panel), 'character')
})

test_that("lineplot() returns appropriately formatted json", {
 
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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contPositive', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- data.table::as.data.table(testDF)
  df$entity.contPositive <- rnorm(500, 10)

  dt <- lineplot.dt(df, variables, value = 'geometricMean', binWidth=0)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','contPositive','repeatedContA'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedContA', 'contPositive', 'cat3', 'cat4'))

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]), c("numeratorN","denominatorN"))
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','cat5','cat2'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat2', 'cat5', 'cat3', 'cat4'))

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]), c("numeratorN","denominatorN"))
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','cat5','repeatedContA'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedContA', 'cat5', 'cat3', 'cat4'))

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean', binWidth=0)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','contB','repeatedContA'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedContA', 'contB', 'cat3', 'cat4'))

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean', binWidth=0)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','contB','repeatedDateA'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedDateA', 'contB', 'cat3', 'cat4'))

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','cat3','contB','repeatedContA'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('repeatedContA', 'contB', 'cat3', 'cat4'))


  # Collection var and computed variable metadata  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(jsonList$lineplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$lineplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      displayName = "Label",
      displayRangeMin = 0.5,
      displayRangeMax = 1.5,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars','viewport','binSlider','binSpec'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(jsonList$lineplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$lineplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars', 'binStart', 'binEnd'))
  expect_equal(names(jsonList$lineplot$data$overlayVariableDetails),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]),"N")
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$lineplot$config$binSlider),c('min','max','step'))
  expect_equal(jsonList$lineplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$lineplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)


  # When we have only one data point and the plot has only one group, ensure seriesX and seriesY
  # will be boxed in json
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, binWidth=100) # Will produce one point
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(typeof(jsonList$lineplot$data$seriesX), 'list')
  expect_equal(typeof(jsonList$lineplot$data$seriesY), 'list')


  # With continuous overlay (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))                  

  dt <- lineplot.dt(df, variables, value = 'proportion', numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('lineplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$lineplot),c('data','config'))
  expect_equal(names(jsonList$lineplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(names(jsonList$lineplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$lineplot$data$facetVariableDetails), 24)
  expect_equal(jsonList$lineplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$lineplot$data$binSampleSize[[1]]), c("numeratorN","denominatorN"))
  expect_equal(names(jsonList$lineplot$data$errorBars[[1]]), c('lowerBound', 'upperBound', 'error'))
  expect_equal(names(jsonList$lineplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$lineplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$lineplot$config$variables$variableSpec$variableId, c('cat4','int6','cat5','cat2'))
  expect_equal(jsonList$lineplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$lineplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat2', 'cat5', 'int6', 'cat4'))
  
})

test_that("lineplot.dt() returns correct information about missing data", {

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  dt <- lineplot.dt(df, variables, value = 'mean')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- lineplot.dt(df, variables, value = 'mean', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.repeatedContA) & !is.na(df$entity.contB)))
  #dt <- lineplot.dt(df, variables, value = 'mean', evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))

  ## Using naToZero to change some NAs to 0
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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      imputeZero = TRUE)
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contA' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.repeatedContA', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- lineplot.dt(df, variables, value = 'mean', evilMode='strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB)))


  ## Collection vars
  # Add nMissing missing values to each column
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity"),
        new("VariableSpec", variableId = "contD", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- lineplot.dt(df, variables, value = 'mean')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)


  ## When an entire part of the num or denom is missing (#157)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
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
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))

  df <- as.data.frame(testDF)
  df$entity.cat2[df$entity.cat2 == 'cat2_a'] <- 'cat2_b'

  # Choose as the numerator the value that is present. Should get all 1s
  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'proportion', binWidth = NULL, numeratorValues = c('cat2_b'), denominatorValues = c('cat2_a','cat2_b'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.cat5', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  expect_true(all(dt$seriesY == 1))

  # Choose as the numerator the value that is NOT present. Should get all 0s
  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'proportion', binWidth = NULL, numeratorValues = c('cat2_a'), denominatorValues = c('cat2_a','cat2_b'))
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.cat5', 'entity.cat4', 'seriesX', 'seriesY', 'binSampleSize', 'errorBars'))
  expect_equal(length(dt$seriesX[[1]]), length(dt$seriesY[[1]]))
  expect_true(all(dt$seriesY == 0))

})

test_that("lineplot.dt() always returns data ordered by seriesX/ binStart", {
  
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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_equal(dt$binStart[[1]], dt$binStart[[1]][order(as.numeric(dt$binStart[[1]]))])

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean')
  expect_equal(dt$binStart[[1]], dt$binStart[[1]][order(as.Date(dt$binStart[[1]]))])

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedDateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'mean', binWidth=0)
  expect_equal(dt$seriesX[[1]], dt$seriesX[[1]][order(as.Date(dt$seriesX[[1]]))])

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
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'repeatedContA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, value = 'median', binWidth=0)
  expect_equal(dt$seriesX[[1]], dt$seriesX[[1]][order(as.numeric(dt$seriesX[[1]]))])

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'ORDINAL'))
  ))

  df <- as.data.frame(testDF)

  dt <- lineplot.dt(df, variables, viewport = NULL, value = 'proportion', binWidth = NULL, numeratorValues = c('cat5_a', 'cat5_b'), denominatorValues = c('cat5_a', 'cat5_b', 'cat5_c', 'cat5_d'))
  expect_equal(dt$seriesX[[1]], dt$seriesX[[1]][order(dt$seriesX[[1]])])
})
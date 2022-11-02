context('scattergl')

test_that("scatter.dt does not fail when there are no complete cases.", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  df <- data.noneComplete[is.na(entity.int),]

  dt <- scattergl.dt(df, variables, 'raw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, variables, value='smoothedMeanWithRaw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, variables, value='bestFitLineWithRaw')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)

  dt <- scattergl.dt(df, variables, value='density')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$densityX), TRUE)
  expect_equal(is.list(dt$densityY), TRUE)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  df <- data.noneComplete

  dt <- scattergl.dt(df, variables, 'raw')
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$seriesX), TRUE)
  expect_equal(is.list(dt$seriesY), TRUE)
})

test_that("scattergl.dt() returns a valid plot.data scatter object", {

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'scatterplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 4)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 12)
})

test_that("scattergl.dt() returns plot data and config of the appropriate types", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, variables, 'raw')
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
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
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

})

test_that("scattergl.dt() returns an appropriately sized data.table", { 

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$entity.dateA)), range(dt$seriesX))

  dt <- scattergl.dt(df, variables, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, variables, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'seriesX', 'seriesY'))
  numericSeriesX <- lapply(dt$seriesX, as.numeric)
  expect_equal(range(df$entity.contA), range(numericSeriesX))

  dt <- scattergl.dt(df, variables, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, variables, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  dt <- scattergl.dt(df, variables, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'densityX', 'densityY'))

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, variables, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
 
  dt <- scattergl.dt(df, variables, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
 
  dt <- scattergl.dt(df, variables, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'densityX', 'densityY'))

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, variables, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, variables, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, variables, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'densityX', 'densityY'))
  
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY'))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, variables, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
    
  dt <- scattergl.dt(df, variables, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, variables, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('densityX', 'densityY'))

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))
  
  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_true(identical(dt$seriesGradientColorscale[[1]], as.character(df$entity.contC)))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))
  
  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))
  
  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('entity.cat4', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))

  ## Collection vars
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity'))
    )),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.collection', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.collection), c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity'))
    )),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY'))
  expect_equal(dt$panel[1], 'cat3_a.||.contA')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet2')@variableId, 'collection')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      displayName = "Peilou's Evenness",
      displayRangeMin = 0,
      displayRangeMax = 1,
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity'))
    )),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.collection', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.collection), c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'overlay')@variableId, 'collection')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  collectionVM <- veupathUtils::findVariableMetadataFromPlotRef(attr(dt, 'variables'), 'yAxis')
  expect_equal(collectionVM@displayRangeMin, 0)
  expect_equal(collectionVM@displayRangeMax, 1)
  expect_equal(collectionVM@displayName, "Peilou's Evenness values")
  collectionVM <- veupathUtils::findVariableMetadataFromPlotRef(attr(dt, 'variables'), 'overlay')
  expect_equal(collectionVM@displayName, "Peilou's Evenness")
  
  # Only one var in the collectionVar
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'))
    )),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt), c('entity.collection', 'entity.cat3', 'seriesX', 'seriesY'))
  expect_equal(unique(dt$entity.collection), c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'overlay')@variableId, 'collection')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues') 
  
  # With factors
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('entity.cat3', 'entity.factor3','seriesX','seriesY'))
  expect_equal(class(dt$entity.factor3), 'character')

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 18)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')

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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  expect_equal(nrow(dt), 9)
  expect_equal(names(dt), c('panel','seriesX','seriesY'))
  expect_equal(class(dt$panel), 'character')
})

test_that("scattergl() returns appropriately formatted json", {
  
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  df <- as.data.frame(testDF)

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 12)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$scatterplot$config$variables),c("variableClass","variableSpec","plotReference","dataType","dataShape","isCollection","imputeZero"))
  expect_equal(jsonList$scatterplot$config$variables$variableSpec$variableId, c("contB","contA","cat3","cat4"))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'contB', 'cat3', 'cat4'))
  
  # Continuous overlay with > 8 values
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 4)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$scatterplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$variables$variableSpec$variableId[jsonList$scatterplot$config$variables$plotReference == 'overlay'], 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))

  # Continuous overlay with < 9 values
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  dt <- scattergl.dt(df, variables, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 24)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$scatterplot$config$variables),c("variableClass","variableSpec","plotReference","dataType","dataShape","isCollection","imputeZero"))
  expect_equal(jsonList$scatterplot$config$variables$variableSpec$variableId, c("contB","contA","int6","cat4"))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[1], 'int6')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId')) 
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','int6','cat4'))

  # With collection vars and computed variable metadata
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(jsonList$scatterplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$scatterplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      displayName = "Label",
      displayRangeMin = 0.5,
      displayRangeMax = 1.5,
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(jsonList$scatterplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$scatterplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contD', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw', collectionVariablePlotRef = 'overlayVariable', computedVariableMetadata = computedVariableMetadata)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('overlayVariableDetails','facetVariableDetails','seriesX','seriesY'))
  expect_equal(names(jsonList$scatterplot$data$overlayVariableDetails),c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(jsonList$scatterplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$scatterplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(length(jsonList$completeCasesTable$variableDetails$variableId), 5)


  # When we have only one data point and the plot has only one group, ensure seriesX and seriesY
  # will be arrays
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  df <- as.data.frame(testDF)
  df <- df[1, ]

  dt <- scattergl.dt(df, variables, 'raw')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(typeof(jsonList$scatterplot$data$seriesX), 'list')
  expect_equal(typeof(jsonList$scatterplot$data$seriesY), 'list')
})



test_that("scattergl.dt() returns correct information about missing data", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    )
  ))

  # Add nMissing missing values to each column
  set.seed(123)
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  dt <- scattergl.dt(df, variables, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- scattergl.dt(df, variables, value = 'raw', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA) & !is.na(df$entity.contB)))
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], length(unlist(dt$seriesX)))
  #dt <- scattergl.dt(df, variables, value = 'raw', evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))

  ## Using naToZero to change some NAs to 0
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      imputeZero = TRUE
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = "STRING"),
      dataShape = new("DataShape", value = "CATEGORICAL")
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contA' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contA', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- scattergl.dt(df, variables, value = 'raw', evilMode='strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB)))
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], length(unlist(dt$seriesX)))

  # SeriesGradientColorscale with no data
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw', evilMode = 'strataVariables')
  expect_equal(nrow(dt), 2)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(lapply(dt$seriesGradientColorscale, length), lapply(dt$seriesX, length))
  expect_true(all(is.na(dt$seriesGradientColorscale[[2]])))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contC', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw', evilMode = 'strataVariables')
  expect_equal(names(dt), c('entity.cat4', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(lapply(dt$seriesGradientColorscale, length), lapply(dt$seriesX, length))
  expect_true(all(is.na(dt$seriesGradientColorscale[[2]])))

  # Testing json output with continuous overlay and no data
  outJson <- getJSON(dt, TRUE)
  jsonList <- jsonlite::fromJSON(outJson) # Turns all nulls in arrays to NA!!
  
  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','seriesX','seriesY','seriesGradientColorscale'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(length(jsonList$scatterplot$data$facetVariableDetails), 10)
  expect_equal(jsonList$scatterplot$data$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_true(all(is.na(jsonList$scatterplot$data$seriesGradientColorscale[[2]]))) # should be null in the json file, but fromJSON converts to NA
  expect_equal(names(jsonList$scatterplot$config),c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$scatterplot$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$scatterplot$config$variables$variableSpec$variableId[jsonList$scatterplot$config$variables$plotReference == 'overlay'], 'contC')
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat4')
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA','contB','contC','cat4'))


  ## Collection vars
  # Add nMissing missing values to each column
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))
  
  # Multiple vars to overlay
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw', collectionVariablePlotRef = 'overlayVariable')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)

  # Multiple vars to facet1
  variables <- new("VariableMetadataList", SimpleList(
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
      dataShape = new("DataShape", value = 'CONTINUOUS')
    )
  ))

  dt <- scattergl.dt(df, variables, 'raw', collectionVariablePlotRef = 'facetVariable1')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)

})

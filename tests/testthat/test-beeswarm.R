context('beeswarm')

test_that("beeswarm.dt() returns a valid plot.data beeswarm object", {
  
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, variables, 0.2, TRUE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'beeswarm')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'beeswarm')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(dt$entity.cat3[[1]], 'cat3_a')
  expect_equal(dt$label[[1]], c('cat4_a','cat4_b','cat4_c','cat4_d'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(42,42,29,51))
})

test_that("beeswarm.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, variables, 0.2, TRUE)
  expect_equal(class(dt$median[[1]]), 'numeric')
  
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- testDF[testDF$entity.cat3 == 'cat3_a' & testDF$entity.cat4 == 'cat4_a',]

  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_equal(class(dt$median[[1]]), 'numeric')

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

test_that("beeswarm.dt() returns an appropriately sized data.table", {
  
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label',  'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
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
  
  dt <- beeswarm.dt(df, variables, 0.2, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  
  dt <- beeswarm.dt(df, variables, 0.2, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  

  # With factors
  df <- testDF
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.factor3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(class(dt$entity.factor3), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),9)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(class(dt$panel), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))

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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(class(dt$panel), 'character')
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  
  ## Collection vars
  # Multiple vars to x
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(unique(dt$label)[[1]], c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'xAxis')@variableId, 'collection')
   
  # Multiple vars to facet1
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.collection', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$entity.collection, c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues', 'median'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$panel[1], 'contA.||.cat4_a')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[3]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[3]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[3]]@displayRangeMax)

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
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$panel[1], 'cat4_a.||.contA')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet2')@variableId, 'collection')  
  
  # Handle only one var sent as a collectionVar
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(unique(dt$label)[[1]], c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'xAxis')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('entity.collection', 'label', 'rawData', 'jitteredValues'))
  expect_equal(length(dt$rawData[[1]][[1]]), length(dt$jitteredValues[[1]][[1]]))
  expect_equal(dt$entity.collection, c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
})

test_that("beeswarm() returns appropriately formatted json", {

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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(jsonList$beeswarm$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      displayName = "yLabel",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues', 'median'))
  expect_equal(jsonList$beeswarm$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal('displayName' %in% names(jsonList$beeswarm$config$variables), TRUE)
  expect_equal(jsonList$beeswarm$config$variables$variableSpec$variableId, c('cat3','contB','cat4'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      displayName = "yLabel",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal('displayName' %in% names(jsonList$beeswarm$config$variables), TRUE)
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal('displayName' %in% names(jsonList$beeswarm$config$variables), TRUE)
  expect_equal(names(jsonList$beeswarm$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

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
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.2, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('label','rawData', 'jitteredValues'))
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  
  # Multiple vars for x and computed variable metadata
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
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "Label",
      displayRangeMin = 0.5,
      displayRangeMax = 1.5,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[2]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[2]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[2]]@displayRangeMax)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
  # Multiple vars to facet1
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "Label",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, TRUE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues', 'median'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[2]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[2]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[2]]@displayRangeMax)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')

  # Multiple vars to facet2
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
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.2, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('facetVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[3]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[3]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[3]]@displayRangeMax)
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 4)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')


  # With continuous overlay variable (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('beeswarm','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$beeswarm), c('data','config'))
  expect_equal(names(jsonList$beeswarm$data), c('overlayVariableDetails','label','rawData', 'jitteredValues'))
  expect_equal(names(jsonList$beeswarm$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$beeswarm$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$beeswarm$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$beeswarm$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$beeswarm$data$label[[1]]), 'character')
  
})


test_that("beeswarm.dt() returns correct information about missing data", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- beeswarm.dt(df, variables, 0.1, TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB) & !is.na(df$entity.cat4))) 
  #dt <- beeswarm.dt(df, variables, 0.1, TRUE, evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))


  ## Using naToZero to change some NAs to 0               
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      imputeZero = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contB' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contB', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- beeswarm.dt(df, variables, 0.1, TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.cat4)))


  ## Collection vars
  # Multiple vars to x

  # Add nMissing missing values to each column -- TODO address that setting na to zero above changes df
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases))
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing)
  expect_true(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1])
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df))


  # Multiple vars to facet1
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
        new("VariableSpec", variableId = 'contB', entityId = 'entity'),
        new("VariableSpec", variableId = 'contA', entityId = 'entity'),
        new("VariableSpec", variableId = 'contC', entityId = 'entity')
      ))
    )
  ))
  
  dt <- beeswarm.dt(df, variables, 0.1, FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases))
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1])
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)
})


test_that("beeswarm.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt_string <- beeswarm.dt(df, variables_string)
  
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt_num <- beeswarm.dt(df, variables_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
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
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt_string <- beeswarm.dt(df, variables_string)
                     
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
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
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt_num <- beeswarm.dt(df, variables_num)
 
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
})

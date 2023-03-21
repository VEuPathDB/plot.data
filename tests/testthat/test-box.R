context('box')

test_that("box.dt does not fail when there are no complete cases.", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- data.noneComplete[is.na(entity.binary1),]

  dt <- box.dt(df, variables, 'none', FALSE, FALSE)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)

  dt <- box.dt(df, variables, 'none', FALSE, TRUE)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)

  dt <- box.dt(df, variables, 'none', TRUE, TRUE)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)
  expect_equal(is.list(dt$mean), TRUE)

  dt <- box.dt(df, variables, 'outliers', FALSE, TRUE)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)
  expect_equal(is.list(dt$outliers), TRUE)
  expect_equal(is.list(dt$outliers[[1]]), TRUE)

  dt <- box.dt(df, variables, 'all', FALSE, TRUE)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)
  expect_equal(is.list(dt$rawData), TRUE)
  expect_equal(is.list(dt$rawData[[1]]), TRUE)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- data.noneComplete

  dt <- box.dt(df, variables, 'none', FALSE, FALSE)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$min), TRUE)
  expect_equal(is.list(dt$q1), TRUE)
  expect_equal(is.list(dt$median), TRUE)
  expect_equal(is.list(dt$q3), TRUE)
  expect_equal(is.list(dt$max), TRUE)
  expect_equal(is.list(dt$lowerfence), TRUE)
  expect_equal(is.list(dt$upperfence), TRUE)
})

test_that("box.dt() returns a valid plot.data box object", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(names(namedAttrList$statsTable), c('entity.cat4','statistic','pvalue','parameter','method','statsError'))
  
  dt <- box.dt(df, variables, 'all', FALSE, computeStats = T)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('entity.cat3','entity.cat4','size'))
  expect_equal(nrow(sampleSizes), 3)
  expect_equal(names(namedAttrList$statsTable), c('entity.cat4','statistic','pvalue','parameter','method','statsError'))
  expect_equal(dt$entity.cat3[[1]], 'cat3_a')
  expect_equal(dt$label[[1]], c('cat4_a','cat4_b','cat4_c','cat4_d'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(42,42,29,51))


  # Ensure sampleSizeTable and completeCasesTable do not get returned if we do not ask for them.
    dt <- box.dt(df, variables, 'all', FALSE, computeStats = T, sampleSizes = FALSE, completeCases = FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables','statsTable'))
  expect_equal(names(namedAttrList$statsTable), c('entity.cat4','statistic','pvalue','parameter','method','statsError'))
  expect_equal(dt$entity.cat3[[1]], 'cat3_a')
  expect_equal(dt$label[[1]], c('cat4_a','cat4_b','cat4_c','cat4_d'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(42,42,29,51))
})

test_that("box.dt() returns plot data and config of the appropriate types", {
  
   variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')
  
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat5)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  
  #w outliers
  dt <- box.dt(df, variables, 'outliers', TRUE)
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')
  #first group has no outliers, want json like [] rather than {}
  expect_equal(class(dt$outliers[[1]][[1]]), 'list')
  expect_equal(class(dt$outliers[[1]][[3]]), 'numeric')
  
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- testDF[testDF$entity.cat3 == 'cat3_a' & testDF$entity.cat4 == 'cat4_a',]

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')

  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.cat4)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')

  # With numeric data for the x axis
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- testDF

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')

  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.int6)), 'integer') ## will become a string when written to json
  expect_equal(class(unlist(sampleSizes$size)), 'integer')

  # With a single numeric box
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- testDF
  df[df$entity.int2 == 2] <- 1

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$q1[[1]]), 'numeric')
  expect_equal(class(dt$median[[1]]), 'numeric')
  expect_equal(class(dt$q3[[1]]), 'numeric')
  expect_equal(class(dt$max[[1]]), 'numeric')
  expect_equal(class(dt$lowerfence[[1]]), 'numeric')
  expect_equal(class(dt$upperfence[[1]]), 'numeric')
  expect_equal(class(dt$mean[[1]]), 'numeric')

  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$entity.int2)), 'numeric') ## will become a string when written to json
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("box.dt() returns an appropriately sized data.table", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- as.data.frame(testDF)
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, variables, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, variables, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, variables, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, variables, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label',  'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, variables, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, variables, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, variables, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
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
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  
  dt <- box.dt(df, variables, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  
  dt <- box.dt(df, variables, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  
  dt <- box.dt(df, variables, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  
  dt <- box.dt(df, variables, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))
  
  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),7)
  expect_equal(names(dt),c('entity.int7', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  
  # Test single outlier
  df <- data.frame('entity.x' = c('group1'), 'entity.y'=c(35.1, 34.2, 36.2, 90.2))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'x', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'y', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- box.dt(df, variables, 'outliers', FALSE, computeStats = T)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  expect_equal(class(dt$label[[1]]), 'character')
  expect_equal(class(dt$min[[1]]), 'numeric')
  expect_equal(class(dt$outliers[[1]]), 'list')
  
  # With factors
  df <- testDF
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.factor3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$entity.factor3), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),9)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$panel), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
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
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- box.dt(df, variables, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))
  expect_equal(class(dt$panel), 'character')
  
  ## Collection vars
  # Multiple vars to x
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'xAxis')@variableId, 'collection')
  
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
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.collection', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.collection, c('contA','contB','contC'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
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
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$panel[1], 'contA.||.cat4_a')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
  index <- which(purrr::map(as.list(attr(dt, 'variables')), function(x) { x@variableSpec@variableId == 'collectionVarValues' }) %in% TRUE)
  collectionVM <- attr(dt, 'variables')[[index]]
  expect_equal(collectionVM@displayName, paste(variables[[1]]@displayName, 'values'))
  expect_equal(collectionVM@displayRangeMin, variables[[1]]@displayRangeMin)
  expect_equal(collectionVM@displayRangeMax, variables[[1]]@displayRangeMax)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 12)
  expect_equal(names(dt),c('panel', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$panel[1], 'cat4_a.||.contA')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet2')@variableId, 'collection')
  
  # Handle only one var sent as a collectionVar
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "Label",
      displayRangeMin = 0,
      displayRangeMax = 1,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'xAxis')@variableId, 'collection')
  
  variables <- new("VariableMetadataList", SimpleList(
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
        new("VariableSpec", variableId = "contB", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('entity.collection', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(dt$entity.collection, c('contB'))
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'yAxis')@variableId, 'collectionVarValues')
  expect_equal(veupathUtils::findVariableSpecFromPlotRef(attr(dt, 'variables'), 'facet1')@variableId, 'collection')
})

test_that("box() returns appropriately formatted json", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- as.data.frame(testDF)

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'cat4')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      displayName = "yLabel",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$boxplot$config$variables$variableSpec$variableId, c('contB','cat3','cat4'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat4')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat4', 'contB', 'cat3'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'cat4')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  # Ensure sampleSizeTable and completeCasesTable are not part of json if we do not ask for them.
  # Make sure to also ask for the stats table to check that all goes well with this special box-specific table
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T, sampleSizes = FALSE, completeCases = FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','statsTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$boxplot$config), c('variables'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$boxplot$config$variables$variableSpec$variableId, c('contB','cat3','cat4'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'cat4')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  


  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      displayName = "yLabel",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$facetVariableDetails[[1]]$variableId, 'cat3')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, TRUE, evilMode = 'strataVariables')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  # no stats table even if requested, when evilMode is 'strataVariables'
  expect_equal(names(jsonList), c('boxplot', 'sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$boxplot$data$overlayVariableDetails), c('variableId','entityId','value'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'binA')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

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
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable), c('xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$statsTable), c('statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$statsTable$statistic), 'integer')
  expect_equal(length(jsonList$statsTable$statistic), 1)
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$statsTable), c('statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(length(jsonList$statsTable$statistic), 1)
  expect_equal(class(jsonList$statsTable$statsError), 'character')

  
  # Multiple vars for x and computed variable metadata
  variables <- new("VariableMetadataList", SimpleList(
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
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$boxplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  # Collection var only
  variables <- new("VariableMetadataList", SimpleList(
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
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity")
      ))
    )
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$boxplot$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$boxplot$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 2)
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')
  
  # Multiple vars to facet1 and computed variable metadata
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'computed'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "Label",
      displayRangeMin = 0.5,
      displayRangeMax = 1.5,
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats=T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$box$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$box$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

  # Multiple vars to facet2 and computed variable metadata
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats=T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('facetVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(jsonList$box$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$box$config$completeCasesAxesVars, nrow(df))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$sampleSizeTable), c('facetVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 4)
  expect_equal(names(jsonList$statsTable), c('facetVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')


  # With continuous overlay (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$boxplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$box$config$completeCasesAllVars, nrow(df))
  expect_equal(jsonList$box$config$completeCasesAxesVars, nrow(df))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(nrow(jsonList$completeCasesTable), 3)
  expect_equal(names(jsonList$statsTable), c('xVariableDetails','statistic','pvalue','parameter','method','statsError'))
  expect_equal(jsonList$statsTable$xVariableDetails$variableId[1], 'binA')
  expect_equal(class(jsonList$statsTable$statistic), 'numeric')
  expect_equal(class(jsonList$statsTable$statsError), 'character')
  expect_equal(class(jsonList$boxplot$data$label[[1]]), 'character')

})


test_that("box.dt() returns correct information about missing data", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- box.dt(df, variables, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- box.dt(df, variables, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contB) & !is.na(df$entity.cat4))) 


  ## Using naToZero to change some NAs to 0
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      imputeZero = TRUE),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contB' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contB', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- box.dt(df, variables, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.cat4)))
  # TODO box cant have evilMode = 'allVariables' bc we cant take median of NA for ex
  #dt <- box.dt(df, variables, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))

  ## Collection vars
  # Multiple vars to x

  # Add nMissing missing values to each column -- TODO address that setting na to zero above changes df
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
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
      variableSpec = new("VariableSpec", variableId = 'collection', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      isCollection = TRUE,
      members = new("VariableSpecList", SimpleList(
        new("VariableSpec", variableId = "contB", entityId = "entity"),
        new("VariableSpec", variableId = "contA", entityId = "entity"),
        new("VariableSpec", variableId = "contC", entityId = "entity")
      ))
    ),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases))
  expect_true(attr(dt, 'completeCasesAllVars')[1] == nrow(df) - nMissing) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1])
  expect_true(attr(dt, 'completeCasesAxesVars')[1] == nrow(df) - nMissing)

})


test_that("box.dt() returns an appropriately sized statistics table", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- as.data.frame(testDF)
  
  ## Kruskal-Wallis
  # No overlay, no facets
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 5)
  expect_equal(names(statsTable), c('statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method[[1]]), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError[[1]]), c('scalar', 'character'))
  
  # No overlay, one facet
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
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay, no facets
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat6))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat6', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay and facet
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
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
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat6)*uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 7)
  expect_equal(names(statsTable), c('entity.cat6', 'entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  ## Wilcoxon
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

  # No overlay, no facets
  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 5)
  expect_equal(names(statsTable), c('statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue[[1]]), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method[[1]]), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError[[1]]), c('scalar', 'character'))
  
  # No overlay, one facet
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
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay, no facets
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.binA))
  expect_equal(ncol(statsTable), 6)
  expect_equal(names(statsTable), c('entity.binA', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
  
  # With overlay and facet
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contB', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
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
      variableSpec = new("VariableSpec", variableId = 'binA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt <- box.dt(df, variables, 'none', FALSE, computeStats = T)
  statsTable <- statsTable(dt)
  expect_equal(nrow(statsTable), uniqueN(df$entity.binA)*uniqueN(df$entity.cat4))
  expect_equal(ncol(statsTable), 7)
  expect_equal(names(statsTable), c('entity.binA', 'entity.cat4', 'statistic', 'pvalue', 'parameter', 'method', 'statsError'))
  expect_equal(class(statsTable$statistic), c('scalar', 'numeric'))
  expect_equal(class(statsTable$pvalue), c('scalar', 'numeric'))
  expect_equal(class(statsTable$method), c('scalar', 'character'))
  expect_equal(class(statsTable$statsError), c('scalar', 'character'))
})



test_that("box.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt_string <- box.dt(df, variables_string)
  
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt_num <- box.dt(df, variables_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt_string <- box.dt(df, variables_string)
  
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'yAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt_num <- box.dt(df, variables_num)
  
  expect_equal(nrow(dt_string), nrow(dt_num))
  expect_equal(names(dt_string), names(dt_num))
  expect_equal(lapply(dt_string, function(x) {length(x[[1]])}), lapply(dt_num, function(x) {length(x[[1]])}))
  
})
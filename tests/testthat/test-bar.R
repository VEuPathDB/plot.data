context('bar')

test_that("bar.dt does not fail when there are no complete cases.", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary1', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  df <- data.noneComplete[is.na(entity.binary1),]

  dt <- bar.dt(df, variables, value='count')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- bar.dt(df, variables, value='proportion')  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)

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
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  df <- data.noneComplete

  dt <- bar.dt(df, variables, value='count')
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(is.list(dt$label), TRUE)
  expect_equal(is.list(dt$value), TRUE)
})

test_that("bar.dt() returns a valid plot.data barplot object", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'barplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','entity.cat6','size'))
  expect_equal(nrow(sampleSizes), 12)
})


test_that("bar.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
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

  # Ensure sampleSizeTable and completeCasesTable do not get returned if we do not ask for them.
  dt <- bar.dt(df, variables, value='count', sampleSizes = FALSE, completeCases = FALSE)
  expect_is(dt$label, 'list')
  expect_equal(class(unlist(dt$label)), 'character')
  expect_is(dt$value, 'list')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList), c('variables'))


  # With numeric x
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
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
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- bar.dt(df, variables, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, variables, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  # sum of x counts should sum to 1 for each panel. Checking cat4_a.||.cat3_b
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$panel == 'cat4_a.||.cat3_b']$label), function(v, dt) {dt[dt$panel=='cat4_a.||.cat3_b']$value[[v]][which(dt[dt$panel == 'cat4_a.||.cat3_b']$label[[v]] == df$entity.cat6[1])]}, dt))),1)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat5', 'label', 'value'))
  
  dt <- bar.dt(df, variables, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat5', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, variables, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),20)
  expect_equal(names(dt),c('entity.cat4', 'entity.cat5', 'label', 'value'))
  # sum of x counts should sum to 1 for each panel. Checking cat5_a
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$entity.cat5 == 'cat5_a']$label), function(v, dt) {dt[dt$entity.cat5=='cat5_a']$value[[v]][which(dt[dt$entity.cat5 == 'cat5_a']$label[[v]] == df$entity.cat6[1])]}, dt))),1)

  variables <- new("VariableMetadataList", SimpleList(
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
  
  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'value'))
  
  dt <- bar.dt(df, variables, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, variables, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'label', 'value'))
  # sum of x counts should sum to 1
  expect_equal(sum(unlist(lapply(seq_along(dt$label), function(v, dt) {dt$value[[v]][which(dt$label[[v]] == df$entity.cat6[1])]}, dt))),1)

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  dt <- bar.dt(df, variables, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),42)
  expect_equal(names(dt),c('entity.cat6', 'entity.int7', 'label', 'value'))
  
  dt <- bar.dt(df, variables, value='proportion', barmode='group')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),42)
  expect_equal(names(dt),c('entity.cat6', 'entity.int7', 'label', 'value'))
  # sum of x counts within a group should sum to 1
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)
  
  dt <- bar.dt(df, variables, value='proportion', barmode='stack')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),42)
  expect_equal(names(dt),c('entity.cat6', 'entity.int7', 'label', 'value'))
  # sum of x counts should sum to 1 for each panel. Checking panel 1
  expect_equal(sum(unlist(lapply(seq_along(dt[dt$entity.int7 == '1']$label), function(v, dt) {dt[dt$entity.int7=='1']$value[[v]][which(dt[dt$entity.int7 == '1']$label[[v]] == df$entity.int6[1])]}, dt))),1)

  # With factors
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
     dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),24)
  expect_equal(names(dt),c('entity.cat4', 'entity.factor6', 'label', 'value'))
  expect_equal(class(dt$entity.factor6), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),24)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(class(dt$panel), 'character')

  
})

test_that("bar() returns appropriately formatted json", {
 
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
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
  
  df <- as.data.frame(testDF)

  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat4','cat6','cat5'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[1], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat4', 'cat5'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "xLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      displayName = "facetLabel",
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$barplot$data$overlayVariableDetails$variableId[1], 'cat4')
  expect_equal(names(jsonList$barplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat4','cat6','cat5'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, 'cat5')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(jsonList$sampleSizeTable$xVariableDetails$variableId[[1]], 'cat6')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('cat6', 'cat4', 'cat5'))
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      displayName = "groupLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "panelLabel",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$data$facetVariableDetails[[1]]), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$barplot$data$facetVariableDetails[[1]]$variableId, 'cat5')
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat4','cat6','cat5'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId','displayLabel'))
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'cat4')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')

  # Ensure sampleSizeTable and completeCasesTable are not part of json if we do not ask for them.
  dt <- bar.dt(df, variables, value='count', sampleSizes = FALSE, completeCases = FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot'))
  expect_equal(names(jsonList$barplot$data$overlayVariableDetails), c('variableId','entityId','value','displayLabel'))
  expect_equal(names(jsonList$barplot$data$facetVariableDetails[[1]]), c('variableId','entityId','value','displayLabel'))
  expect_equal(jsonList$barplot$data$facetVariableDetails[[1]]$variableId, 'cat5')
  expect_equal(names(jsonList$barplot$config), c('variables'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat4','cat6','cat5'))

  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('int7','int6','cat6'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'cat6')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'cat6', 'int7'))

  # With a integer overlay 
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'INTEGER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat6','int6','int7'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'int7')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'int7', 'cat6'))

  # With a number overlay (fewer than 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  dt <- bar.dt(df, variables, value='count')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('barplot','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$barplot), c('data','config'))
  expect_equal(names(jsonList$barplot$data), c('overlayVariableDetails','facetVariableDetails','label','value'))
  expect_equal(names(jsonList$barplot$config), c('variables','completeCasesAllVars','completeCasesAxesVars'))
  expect_equal(names(jsonList$barplot$config$variables$variableSpec), c('variableId','entityId'))
  expect_equal(jsonList$barplot$config$variables$variableSpec$variableId, c('cat6','int6','int7'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','facetVariableDetails','xVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$overlayVariableDetails$variableId[[1]], 'int7')
  expect_equal(class(jsonList$sampleSizeTable$xVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId','entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('int6', 'int7', 'cat6'))

})



test_that("bar.dt() returns correct information about missing data", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
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
  
  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  dt <- bar.dt(df, variables, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- bar.dt(df, variables, value='count', evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA)))
  dt <- bar.dt(df, variables, value='count', evilMode = 'allVariables')
  colNames <- unlist(purrr::map(as.list(variables), function(x) {veupathUtils::getColName(x@variableSpec)}))
  expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, colNames, with=FALSE])))
})


test_that("bar.dt() returns same shaped outputs for string cats and num cats.", {
  
  df <- testDF
  
  variables_string <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat7', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt_string <- bar.dt(df, variables_string)
  
  variables_num <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL'))
  ))

  dt_num <- bar.dt(df, variables_num)
  
  expect_equal(dim(dt_string), dim(dt_num))
  expect_equal(names(dt_string), c('entity.cat4', 'entity.cat6', 'label', 'value'))
  expect_equal(names(dt_num), c('entity.cat4', 'entity.int6', 'label', 'value'))
  expect_equal(dt_string$entity.cat4, dt_num$entity.cat4)
  expect_equal(length(dt_string$label[[1]]), length(dt_num$label[[1]]))
  expect_equal(length(dt_string$value[[1]]), length(dt_num$value[[1]]))
  
})
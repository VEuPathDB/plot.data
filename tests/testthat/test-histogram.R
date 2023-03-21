context('histogram')

test_that("histogram.dt does not fail when there are no complete cases.", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- data.noneComplete[is.na(entity.cont),]

  dt <- histogram.dt(df, variables, binWidth = .3, value='count', barmode = 'overlay', 'binWidth', NULL)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', 'binWidth', NULL)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- histogram.dt(df, variables, binWidth = .3, value='proportion', barmode = 'overlay', 'binWidth', NULL)  
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)
    
  dt <- histogram.dt(df, variables, binWidth = .3, value='count', barmode = 'stack', 'binWidth', NULL)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)

  dt <- histogram.dt(df, variables, binWidth = .3, value='count', barmode = 'overlay', 'numBins', NULL)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'binary2', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cont', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))
  
  df <- data.noneComplete

  dt <- histogram.dt(df, variables, binWidth = .3, value='count', barmode = 'overlay', 'binWidth', NULL)
  attr <- attributes(dt)
  expect_equal(attr$completeCasesAllVars[1], 0)
  expect_equal(as.character(attr$viewport$xMin), "")
  expect_equal(is.na(attr$binSlider$min), TRUE)
  expect_equal(as.character(attr$summary$min), "")
  expect_equal(is.na(attr$binSpec$value), TRUE)
  expect_equal(is.list(dt$binLabel), TRUE)
  expect_equal(is.list(dt$binStart), TRUE)
  expect_equal(is.list(dt$binEnd), TRUE)
  expect_equal(is.list(dt$value), TRUE)
})

test_that("histogram.dt() returns requested numBins/ binWidth", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = .3, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(round(as.numeric(binSpec(dt)$value),1), .3)
  expect_equal(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))), .3)

  dt <- histogram.dt(df, variables, binWidth = 1.5, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(round(as.numeric(binSpec(dt)$value),1), 1.5)
  expect_equal(round(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))),1), 1.5)

  binReportValue <- 'numBins'
  binWidth <- numBinsToBinWidth(df$entity.contA, 5)
  dt <- histogram.dt(df, variables, binWidth = binWidth, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(as.numeric(binSpec(dt)$value), 5) # ensure we get the correct number of bins
  expect_true(max(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart)) - binWidth) < 0.1) # Tolerance 0.1
  numericLabelsStart <- unlist(lapply(unlist(dt$binLabel), function(x) as.numeric(stringi::stri_split_regex(x, ",|]|\\(|\\[")[[1]][2])))
  numericLabelsEnd <- unlist(lapply(unlist(dt$binLabel), function(x) as.numeric(stringi::stri_split_regex(x, ",|]|\\(|\\[")[[1]][3])))
  expect_true(max(numericLabelsEnd - numericLabelsStart) - binWidth < 0.1) # Label tolerance

  binReportValue <- 'numBins'
  binWidth <- numBinsToBinWidth(df$entity.contA, 15)
  dt <- histogram.dt(df, variables, binWidth = binWidth, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(as.numeric(binSpec(dt)$value), 15) # ensure we get the correct number of bins
  expect_true(max(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart)) - binWidth) < 0.1) # Tolerance 0.1
  numericLabelsStart <- unlist(lapply(unlist(dt$binLabel), function(x) as.numeric(stringi::stri_split_regex(x, ",|]|\\(|\\[")[[1]][2])))
  numericLabelsEnd <- unlist(lapply(unlist(dt$binLabel), function(x) as.numeric(stringi::stri_split_regex(x, ",|]|\\(|\\[")[[1]][3])))
  expect_true(max(numericLabelsEnd - numericLabelsStart) - binWidth < 0.1) # Label tolerance
})

test_that("histogram.dt() returns a valid plot.data histogram object", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','summary', 'viewport', 'binSpec', 'binSlider'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','size'))
  expect_equal(nrow(sampleSizes), 12)
  expect_equal(names(namedAttrList$summary), c('min','q1','median','mean','q3','max'))
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
  expect_equal(names(namedAttrList$binSpec), c('type','value')) 
  expect_equal(round(as.numeric(namedAttrList$binSpec$value),4), 1.8032)
  expect_equal(as.character(namedAttrList$binSpec$type), 'binWidth')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.dateA), 'xMax'=max(df$entity.dateA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = 'month', value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables', 'completeCasesAllVars','completeCasesAxesVars','completeCasesTable','sampleSizeTable','summary', 'viewport', 'binSpec', 'binSlider'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','size'))
  expect_equal(nrow(sampleSizes), 12)
  expect_equal(names(namedAttrList$summary), c('min','q1','median','mean','q3','max'))
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
  expect_equal(names(namedAttrList$binSpec), c('type','value','units'))
  expect_equal(as.numeric(namedAttrList$binSpec$value),1)
  expect_equal(as.character(namedAttrList$binSpec$type),'binWidth')
  expect_equal(as.character(namedAttrList$binSpec$unit),'month')
  # expect_true(all(grepl('T00:00:00', unlist(dt$binStart))))
  # expect_true(all(grepl('T00:00:00', unlist(dt$binEnd))))
  # expect_true(!any(grepl('T00:00:00', unlist(dt$binLabel))))


  # Ensure sampleSizeTable and completeCasesTable do not get returned if we do not ask for them.
  dt <- histogram.dt(df, variables, binWidth = 'month', value='count', barmode = 'overlay', binReportValue, viewport, sampleSizes = FALSE, completeCases = FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('variables','summary', 'viewport', 'binSpec', 'binSlider'))
  expect_equal(names(namedAttrList$summary), c('min','q1','median','mean','q3','max'))
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
  expect_equal(names(namedAttrList$binSpec), c('type','value','units'))
  expect_equal(as.numeric(namedAttrList$binSpec$value),1)
  expect_equal(as.character(namedAttrList$binSpec$type),'binWidth')
  expect_equal(as.character(namedAttrList$binSpec$unit),'month')
  
})

test_that("histogram.dt() returns plot data and config of the appropriate types", {
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
 
  expect_equal(class(unlist(dt$panel)), 'character') 
  expect_equal(class(unlist(dt$binLabel)), 'character') 
  expect_equal(class(unlist(dt$binStart)), 'character') 
  expect_equal(class(unlist(dt$binEnd)), 'character') 
  expect_equal(class(unlist(dt$value)), 'integer') 
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSpec$value),c('scalar', 'numeric'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.dateA), 'xMax'=max(df$entity.dateA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = 'month', value='count', barmode = 'overlay', binReportValue, viewport)

  expect_equal(class(unlist(dt$panel)), 'character')
  expect_equal(class(unlist(dt$binLabel)), 'character')
  expect_equal(class(unlist(dt$binStart)), 'character')
  expect_equal(class(unlist(dt$binEnd)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCasesAllVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$completeCasesAxesVars),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSpec$value),c('scalar', 'numeric'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("histogram.dt() returns bins according to specified viewport", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  binReportValue <- 'binWidth'
  viewport <- list('xMin'=-1.5,'xMax'=2.5)
  
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  maxBinStart <- as.numeric(max(unlist(lapply(dt$binStart, max))))
  expect_true(maxBinStart <= viewport$xMax)
  minBinEnd <- as.numeric(min(unlist(lapply(dt$binEnd, min))))
  expect_true(minBinEnd >= viewport$xMin)
  
  #figure how to test for expanding to viewport, since we dont explicitly return 0 value bins..
})

test_that("histogram.dt() returns an appropriately sized data.table", {
  df <- as.data.frame(testDF)
  
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport = viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport = viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))


  #this for dates
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.dateA), 'xMax'=max(df$entity.dateA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

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
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.dateA), 'xMax'=max(df$entity.dateA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt),c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'DATE'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))
  
  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'value', 'binStart', 'binEnd'))


  # With factors
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'factor3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt),c('entity.factor3', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(class(dt$entity.factor3), 'character')

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
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
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),9)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(class(dt$panel), 'character')

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
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),18)
  expect_equal(names(dt),c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(class(dt$panel), 'character')

})

test_that("histogram() returns appropriately formatted json", {

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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('overlayVariableDetails','facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(jsonList$histogram$data$overlayVariableDetails$variableId[1], 'cat3')
  expect_equal(names(jsonList$histogram$config),c('variables','completeCasesAllVars','completeCasesAxesVars','summary','viewport','binSpec','binSlider'))  
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$histogram$config$variables$variableSpec$variableId, c('cat3','cat4','contA'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails', 'size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'cat3', 'cat4'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
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
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  df <- as.data.frame(testDF)
  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(jsonList$histogram$data$facetVariableDetails[[1]]$variableId, c('cat4', 'cat3'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(names(jsonList$histogram$config),c('variables','completeCasesAllVars','completeCasesAxesVars','summary','viewport','binSpec','binSlider'))
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$histogram$config$variables$variableSpec$variableId, c('cat3','cat4','contA'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, c('cat4', 'cat3'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId')) 
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'cat4', 'cat3'))
  
 
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(jsonList$histogram$data$facetVariableDetails[[1]]$variableId, c('cat4', 'cat3'))
  expect_equal(names(jsonList$histogram$config),c('variables','completeCasesAllVars','completeCasesAxesVars','summary','viewport','binSpec','binSlider'))
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$histogram$config$variables$variableSpec$variableId, c('cat3','cat4','contA'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(jsonList$sampleSizeTable$facetVariableDetails[[1]]$variableId, c('cat4', 'cat3'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'cat4', 'cat3'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      displayName = "facet2Label",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "facet1Label",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      displayName = "xLabel",
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  
  expect_equal(names(jsonList$histogram$config),c('variables','completeCasesAllVars','completeCasesAxesVars','summary','viewport','binSpec','binSlider'))
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId', 'displayLabel'))

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat3', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet2'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      displayName = "facet1Label",
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId', 'displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')


  # Ensure sampleSizeTable and completeCasesTable are not part of json if we do not ask for them.
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport, sampleSizes = FALSE, completeCases = FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$config),c('variables','summary','viewport','binSpec','binSlider'))
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))


  # With continuous overlay (< 9 values)
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'int6', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  viewport <- list('xMin'=min(df$entity.contA), 'xMax'=max(df$entity.contA))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('overlayVariableDetails','facetVariableDetails','binLabel','value','binStart','binEnd'))
  expect_equal(names(jsonList$histogram$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(jsonList$histogram$data$overlayVariableDetails$variableId[1], 'int6')
  expect_equal(names(jsonList$histogram$config),c('variables','completeCasesAllVars','completeCasesAxesVars','summary','viewport','binSpec','binSlider'))  
  expect_equal(names(jsonList$histogram$config$variables$variableSpec),c('variableId','entityId'))
  expect_equal(jsonList$histogram$config$variables$variableSpec$variableId, c('int6','cat4','contA'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails', 'size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId'))
  expect_equal(jsonList$completeCasesTable$variableDetails$variableId, c('contA', 'int6', 'cat4'))
  
})

test_that("histogram.dt() returns correct information about missing data", {

  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  # Add nMissing missing values to each column
  nMissing <- 10
  df <- as.data.frame(lapply(testDF, function(x) {x[sample(1:length(x), nMissing, replace=F)] <- NA; x}))

  viewport <- list('xMin'=min(testDF$entity.contA), 'xMax'=max(testDF$entity.contA))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - nMissing
  expect_equal(all(completecasestable$completeCases == nrow(df)-nMissing), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCasesAllVars')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'completeCasesAxesVars')[1] >= attr(dt, 'completeCasesAllVars')[1], TRUE)
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', binReportValue = binReportValue, viewport = viewport, evilMode = 'strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], sum(!is.na(df$entity.contA)))
  #dt <- histogram.dt(df, variables, binWidth = NULL, value='count', binReportValue = binReportValue, viewport = viewport, evilMode = 'allVariables')
  #expect_equal(attr(dt, 'completeCasesAllVars')[1], sum(complete.cases(df[, map$id, with=FALSE])))

  ## Using naToZero to change some NAs to 0
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'cat5', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'overlay'),
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
      variableSpec = new("VariableSpec", variableId = 'cat4', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'facet1'),
      dataType = new("DataType", value = 'STRING'),
      dataShape = new("DataShape", value = 'CATEGORICAL')),
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'),
      imputeZero = TRUE)
  ))

  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  completecasestable <- completeCasesTable(dt)
  # Each entry except 'contB' should equal NROW(df) - nMissing
  expect_equal(sum(completecasestable$completeCases == nrow(df)-nMissing), 3)
  expect_equal(completecasestable[variableDetails=='entity.contA', completeCases], nrow(df))
  # number of completeCases should be < complete cases for each var
  expect_true(all(attr(dt, 'completeCasesAllVars')[1] < completecasestable$completeCases)) 
  expect_true(attr(dt, 'completeCasesAxesVars')[1] > attr(dt, 'completeCasesAllVars')[1])
  dt <- histogram.dt(df, variables, binWidth = NULL, value='count', binReportValue = binReportValue, viewport = viewport, evilMode='strataVariables')
  expect_equal(attr(dt, 'completeCasesAxesVars')[1], nrow(df))
})

test_that("histogram.dt does not produce corrupted bins when given TAC data from issue 182", {
  variables <- new("VariableMetadataList", SimpleList(
    new("VariableMetadata",
      variableClass = new("VariableClass", value = 'native'),
      variableSpec = new("VariableSpec", variableId = 'values', entityId = NA_character_),
      plotReference = new("PlotReference", value = 'xAxis'),
      dataType = new("DataType", value = 'NUMBER'),
      dataShape = new("DataShape", value = 'CONTINUOUS'))
  ))

  dt <- histogram.dt(data.issue182, variables, binWidth = 1, value = 'count', barmode = 'stack', binReportValue = 'binWidth')

  # check that there are no NAs (which the client is getting as nulls) in the binLabels
  expect_equal(sum(is.na(unlist(dt$binLabel))), 0)
  # check that the binStart and binEnd strings are all valid numbers
  # (the bug gives values like '24]')
  expect_equal(sum(is.na(as.numeric(unlist(dt$binStart)))), 0)
  expect_equal(sum(is.na(as.numeric(unlist(dt$binEnd)))), 0)
})

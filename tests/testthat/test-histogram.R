context('histogram')

test_that("histogram.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'incompleteCases','sampleSizeTable','facetVariable1', 'facetVariable2', 'summary', 'viewport', 'binSlider', 'binSpec'))
  expect_equal(class(namedAttrList$incompleteCases),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))

  viewport <- list('xMin'=-1.5,'xMax'=2.5)
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  maxBinStart <- as.numeric(max(unlist(lapply(dt$binStart, max))))
  expect_true(maxBinStart <= viewport$xMax)
  minBinEnd <- as.numeric(min(unlist(lapply(dt$binEnd, min))))
  expect_true(minBinEnd >= viewport$xMin)

  #figure how to test for expanding to viewport, since we dont explicitly return 0 value bins..

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)


  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('var', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('var'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('NUMBER'), 'dataShape' = c('CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))


  #this for dates, split into its own test?
  map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.dates)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = 'month', value='count', binReportValue, viewport)
  namedAttrList <- getPDAttributes(dt)
  expect_equal(as.numeric(namedAttrList$binSpec$value),1)
  expect_equal(as.character(namedAttrList$binSpec$type),'binWidth')
  expect_equal(as.character(namedAttrList$binSpec$unit),'month')

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'incompleteCases','sampleSizeTable','facetVariable1', 'facetVariable2', 'summary', 'viewport', 'binSlider', 'binSpec'))
  expect_equal(class(namedAttrList$incompleteCases),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSpec$value),c('scalar', 'numeric'))


  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)


  map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.dates)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('group', 'date'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'DATE'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('date', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), 'dataType' = c('DATE', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('date'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('DATE'), 'dataShape' = c('CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

})

test_that("histogram() returns consistent and appropriately formatted json", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  outJson <- getJSON(dt)
  expect_equal_to_reference(outJson, 'histogramJson.rds')
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('overlayVariableDetails','facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$histogram$config),c('incompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))  
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails', 'size'))
  

  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  outJson <- getJSON(dt)
  expect_equal_to_reference(outJson, 'histogramJson.twoFacets.rds')
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(names(jsonList$histogram$config),c('incompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
})

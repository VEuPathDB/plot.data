context('histogram')

test_that("histogram.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
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


  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('var', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'STRING'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('var'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('NUMBER'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  #this for dates, split into its own test?
  map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), stringsAsFactors=FALSE)
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


  map <- data.frame('id' = c('group', 'date'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'DATE'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('date', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), 'dataType' = c('DATE', 'STRING'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('date'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('DATE'), stringsAsFactors = FALSE)

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
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  outJson <- getJSON(dt)
  expect_equal_to_reference(outJson, 'histogram.json')
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('data','config'))
  expect_equal(names(jsonList$data),c('overlayVariableDetails','panel','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$config),c('incompleteCases','summary','viewport','binSlider','binWidth','xVariableDetails'))  
  expect_equal(names(jsonList$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$config$summary),c('min','q1','median','mean','q3','max'))
})

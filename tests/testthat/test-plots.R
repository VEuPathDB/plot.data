context('plots')

# TODO figure refactoring while still having informative errors

test_that("scattergl() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'none')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'series.y', 'series.x'))

  dt <- scattergl.dt(df, map, 'smoothedMean')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))


  map <- data.frame('id' = c('group', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x'))

  dt <- scattergl.dt(df, map, 'smoothedMean')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))


  map <- data.frame('id' = c('y', 'x', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x'))

  dt <- scattergl.dt(df, map, 'smoothedMean')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))

  map <- data.frame('id' = c('y', 'x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x'))

  dt <- scattergl.dt(df, map, 'smoothedMean')[[1]]
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))

  #TODO test w two facets
})


test_that("histogram() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('x.min'=min(bigData$var), 'x.max'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'value'))


  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'value'))


  map <- data.frame('id' = c('var', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'STRING'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'value'))

  map <- data.frame('id' = c('var'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('NUMBER'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'value'))

  #TODO test w two facets
})

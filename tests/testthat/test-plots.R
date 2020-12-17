context('plots')

# TODO figure refactoring while still having informative errors

test_that("scattergl() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'none')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'series.y', 'series.x'))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))


  map <- data.frame('id' = c('group', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x', 'panel'))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'panel'))


  map <- data.frame('id' = c('y', 'x', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x', 'group'))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'group'))

  map <- data.frame('id' = c('y', 'x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'none')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x', 'group', 'panel'))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'group', 'panel'))

  #TODO test w two facets
})


test_that("histogram() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)

  dt <- histogram.dt(df, map, binWidth = NULL, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'binLabel', 'binStart', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'binLabel', 'binStart', 'value'))


  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'value', 'panel'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'value', 'panel'))


  map <- data.frame('id' = c('var', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'value', 'group'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'value', 'group'))

  map <- data.frame('id' = c('var'), 'plotRef' = c('xAxisVariable'), stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'value', 'group', 'panel'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'value', 'group', 'panel'))

  #TODO test w two facets
})

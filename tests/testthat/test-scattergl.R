context('scattergl')

test_that("scattergl.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'series.dependent', 'series.independent'))

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('interval.independent', 'interval.dependent', 'interval.se', 'group', 'panel'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'series.dependent', 'series.independent', 'interval.independent', 'interval.dependent', 'interval.se'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'density.independent', 'density.dependent'))

  map <- data.frame('id' = c('group', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.dependent', 'series.independent'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.dependent', 'series.independent', 'interval.independent', 'interval.dependent', 'interval.se'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('interval.independent', 'interval.dependent', 'interval.se', 'group'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'density.independent', 'density.dependent'))


  map <- data.frame('id' = c('y', 'x', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.dependent', 'series.independent'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.dependent', 'series.independent', 'interval.independent', 'interval.dependent', 'interval.se'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('interval.independent', 'interval.dependent', 'interval.se', 'panel'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'density.independent', 'density.dependent'))
  

  map <- data.frame('id' = c('y', 'x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.dependent', 'series.independent'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.dependent', 'series.independent', 'interval.independent', 'interval.dependent', 'interval.se'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('interval.independent', 'interval.dependent', 'interval.se'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('density.independent', 'density.dependent'))
})

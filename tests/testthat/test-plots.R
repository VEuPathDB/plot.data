context('plots')

# TODO figure refactoring while still having informative errors

test_that("scattergl() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.xy)

  dt <- scattergl(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'series.y', 'series.x'))

  dt <- scattergl(df, map, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se'))


  map <- data.frame('id' = c('group', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x', 'panel'))

  dt <- scattergl(df, map, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'panel'))


  map <- data.frame('id' = c('y', 'x', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors = FALSE)

  dt <- scattergl(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x', 'group'))

  dt <- scattergl(df, map, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'group'))

  map <- data.frame('id' = c('y', 'x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- scattergl(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x', 'group', 'panel'))

  dt <- scattergl(df, map, TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('series.y', 'series.x', 'interval.x', 'interval.y', 'interval.se', 'group', 'panel'))

  #TODO test w two facets
})


test_that("histogram() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)

  dt <- histogram(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'x', 'y'))

  dt <- histogram(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'x', 'y'))


  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), stringsAsFactors = FALSE)

  dt <- histogram(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'x', 'y', 'panel'))

  dt <- histogram(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'x', 'y', 'panel'))


  map <- data.frame('id' = c('var', 'panel'), 'plotRef' = c('xAxisVariable', 'facetVariable1'), stringsAsFactors = FALSE)

  dt <- histogram(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'x', 'y', 'group'))

  dt <- histogram(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'x', 'y', 'group'))

  map <- data.frame('id' = c('var'), 'plotRef' = c('xAxisVariable'), stringsAsFactors = FALSE)

  dt <- histogram(df, map)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('x', 'y', 'group', 'panel'))

  dt <- histogram(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('x', 'y', 'group', 'panel'))

  #TODO test w two facets
})

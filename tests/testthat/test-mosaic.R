context('mosaic')

test_that("mosaic.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt, 'mosaic')
  expect_is(dt$dependent, 'list')
  expect_is(dt$dependent[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('oddsratio', 'p.value', 'independent.label', 'or.interval', 'dependent.label', 'dependent', 'relativerisk', 'rr.interval', 'panel'))

  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt, 'mosaic')
  expect_is(dt$dependent, 'list')
  expect_is(dt$dependent[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('oddsratio', 'p.value', 'independent.label', 'or.interval', 'dependent.label', 'dependent', 'relativerisk', 'rr.interval'))
})

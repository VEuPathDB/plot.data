context('mosaic')

test_that("mosaic.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), stringsAsFactors=FALSE)

#TODO for this and other plot types add test of type of cols
# for list type cols add test for length etc
  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt, 'mosaic')
  expect_is(dt$y, 'list')
  expect_is(dt$y[[1]], 'list')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('oddsratio', 'p.value', 'x', 'or.interval', 'y.label', 'y', 'relativerisk', 'rr.interval', 'panel'))

  map <- data.frame('id' = c('group', 'var'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), stringsAsFactors=FALSE)

  dt <- mosaic.dt(data.binary, map)
  expect_is(dt, 'data.table')
  expect_is(dt, 'mosaic')
  expect_is(dt$y, 'list')
  expect_is(dt$y[[1]], 'list')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('oddsratio', 'p.value', 'x', 'or.interval', 'y.label', 'y', 'relativerisk', 'rr.interval'))

  # TODO test w two facets

  #TODO test rxc
})

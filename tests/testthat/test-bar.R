context('bar')

test_that("bar.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'independentVar', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'independentVar', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))

  map <- data.frame('id' = c('group', 'x'), 'plotRef' = c('overlayVariable', 'independentVar'), 'dataType' = c('STRING', 'STRING'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))

  map <- data.frame('id' = c('x'), 'plotRef' = c('independentVar'), 'dataType' = c('STRING'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
})

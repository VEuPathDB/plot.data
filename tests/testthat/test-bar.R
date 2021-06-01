context('bar')

test_that("bar.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  expect_equal(class(unlist(dt$label)),'character')
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'label', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)

  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_is(dt, 'barplot')
  expect_is(dt$label, 'list')
  expect_is(dt$value, 'list')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'label', 'value'))
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)

  map <- data.frame('id' = c('group', 'x'), 'plotRef' = c('overlayVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'value'))
  expect_equal(all(lapply(dt$value, sum) == 1), TRUE)

  map <- data.frame('id' = c('x'), 'plotRef' = c('xAxisVariable'), 'dataType' = c('STRING'), 'dataShape' = c('CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- bar.dt(df, map, value='count')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
  
  dt <- bar.dt(df, map, value='proportion')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'value'))
})

test_that("bar.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.binned)
  
  # Add 10 missing values to each column
  df$x[sample(1:100, 10, replace=F)] <- NA
  df$y[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  dt <- bar.dt(df, map, value='count')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of incompleteCases should be <= sum of incomplete cases within each var
  expect_equal(attr(dt, 'incompleteCases')[1] <= sum(nrow(df) - completecasestable$completeCases), TRUE)
  
})

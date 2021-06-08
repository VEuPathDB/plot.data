context('box')

test_that("box.dt() returns an appropriately sized data.table", {
  #using panel for xaxis.. maybe make another test.dt for this?
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'seriesX', 'seriesY'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'seriesX', 'seriesY', 'mean'))


  map <- data.frame('id' = c('y', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel',  'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'seriesX', 'seriesY'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'seriesX', 'seriesY', 'mean'))
  
  ## Case when we input multiple vars as one to x axis
  map <- data.frame('id' = c('y', 'x', 'z', 'group'), 'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  df[, z := x+y]
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt),c('group', 'meltedVariable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$meltedVariable)[[1]], c('x','y','z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'meltedVariable')
  
  ## Case when we input multiple vars as one to facet 1
  map <- data.frame('id' = c('y', 'x', 'z', 'group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('meltedVariable', 'group', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$meltedVariable), c('x','y','z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'meltedVariable')
  
})

test_that("box.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  # Add 10 missing values to each column
  df$x[sample(1:100, 10, replace=F)] <- NA
  df$y[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  dt <- box.dt(df, map, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of incompleteCases should be <= sum of incomplete cases within each var
  expect_equal(attr(dt, 'incompleteCases')[1] <= sum(nrow(df) - completecasestable$completeCases), TRUE)
  
})

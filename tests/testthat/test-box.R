context('box')

test_that("box.dt() returns an appropriately sized data.table", {
  
  # Ordered box testing - do group and panel, and just y,x
  map <- data.frame('id' = c('y, x', 'group', 'panel'),
                    'plotRef' = c('xAxisVariable', 'overlayVariable', 'facetVariable1'),
                    'dataType' = c('NUMBER, NUMBER', 'STRING', 'STRING'),
                    stringsAsFactors=FALSE)
  df <- data.xy
  dt <- box.dt(df, map, 'none', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  
  dt <- box.dt(df, map, 'outliers', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  
  dt <- box.dt(df, map, 'all', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))
  
  dt <- box.dt(df, map, 'all', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt),c('panel', 'group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))

  # Ordered box test using only values for x axis
  map <- data.frame('id' = c('y, x'),
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('NUMBER, NUMBER'),
                    stringsAsFactors = FALSE)
  
  dt <- box.dt(df, map, 'none', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  
  dt <- box.dt(df, map, 'none', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  
  dt <- box.dt(df, map, 'outliers', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  
  dt <- box.dt(df, map, 'outliers', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  
  dt <- box.dt(df, map, 'all', FALSE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))
  
  dt <- box.dt(df, map, 'all', TRUE, ', ')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt),c('variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))
  
  
  
  #using panel for xaxis.. maybe make another test.dt for this?
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))

  dt <- box.dt(df, map, 'all', TRUE, independentDelimiter=NULL)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))


  map <- data.frame('id' = c('y', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), stringsAsFactors=FALSE)

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
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))
})

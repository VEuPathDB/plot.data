context('orderedBox')

test_that("orderedBox.dt() returns an appropriately sized data.table", {
  #using panel for xaxis.. maybe make another test.dt for this?
  #### QUESTION -- should we be okay with NULL or empty entityIds?
  map <- data.frame('id' = c('group', 'y, x'), 'plotRef' = c('overlayVariable', 'xAxisList'), 'dataType' = c('STRING', 'NUMBER, NUMBER'), stringsAsFactors=FALSE)
  df <- data.xy
  
  dt <- orderedBox.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  
  dt <- orderedBox.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  # 
  dt <- orderedBox.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'variable', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  # 
  # dt <- box.dt(df, map, 'outliers', TRUE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),4)
  # expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  # 
  # dt <- box.dt(df, map, 'all', FALSE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),4)
  # expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))
  # 
  # dt <- box.dt(df, map, 'all', TRUE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),4)
  # expect_equal(names(dt),c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))
  # 
  # 
  # map <- data.frame('id' = c('y', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), stringsAsFactors=FALSE)
  # 
  # dt <- box.dt(df, map, 'none', FALSE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  # 
  # dt <- box.dt(df, map, 'none', TRUE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel',  'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))
  # 
  # dt <- box.dt(df, map, 'outliers', FALSE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))
  # 
  # dt <- box.dt(df, map, 'outliers', TRUE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))
  # 
  # dt <- box.dt(df, map, 'all', FALSE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y'))
  # 
  # dt <- box.dt(df, map, 'all', TRUE)
  # expect_is(dt, 'data.table')
  # expect_equal(nrow(dt),1)
  # expect_equal(names(dt),c('panel', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'series.x', 'series.y', 'mean'))
})

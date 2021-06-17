context('box')

test_that("box.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE)
  expect_equal(class(unlist(dt$min)), 'numeric')
  expect_equal(class(unlist(dt$q1)), 'numeric')
  expect_equal(class(unlist(dt$median)), 'numeric')
  expect_equal(class(unlist(dt$q3)), 'numeric')
  expect_equal(class(unlist(dt$max)), 'numeric')
  expect_equal(class(unlist(dt$lowerfence)), 'numeric')
  expect_equal(class(unlist(dt$upperfence)), 'numeric')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$incompleteCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("box.dt() returns an appropriately sized data.table", {
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

test_that("box.dt() returns an appropriately sized statistics table", {
  map <- data.frame('id' = c('xcat', 'y'), 'plotRef' = c('xAxisVariable', 'yAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.xy)
  df$xcat <- sample(c('x1','x2','x3'), 500, replace=T) # Add another categorical var
  
  # No overlay, no facets
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 1)
  
  # No overlay, one facet
  map <- data.frame('id' = c('xcat', 'y', 'panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$panel))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay, no facets
  map <- data.frame('id' = c('xcat', 'y', 'group'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$xcat))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay and facet
  map <- data.frame('id' = c('xcat', 'y', 'group', 'panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$xcat)*uniqueN(df$panel))
  expect_equal(ncol(statsTable), 3)
  
  
})

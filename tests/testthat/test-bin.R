context('bin')

test_that("binProportion() returns an appropriately sized data.table", {
  viewport <- list('xMin'=min(data.xy$y), 'xMax'=max(data.xy$y)) 
  df <- data.xy[, 'y']

  dt <- binProportion(df,'y', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(df,'y', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)

  df <- data.xy[, c('y','group')]

  dt <- binProportion(df, 'y','group', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'y','group', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt$binLabel), 'value'=unlist(dt$value))
  expect_true(all(dt_inter[, sum(value), by='binLabel'][['V1']] == 1))

  df <- data.xy[, c('y','panel')]

  dt <- binProportion(df, 'y', NULL, 'panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'y', NULL, 'panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))

  df <- data.xy[, c('y','group','panel')]

  dt <- binProportion(df, 'y', 'group', 'panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('group', 'panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt[dt$panel == 'panel1']$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'y', 'group', 'panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('group', 'panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))
  

})

test_that("binProportion() returns consistent results", {
  viewport = list('xMin'=min(data.xy$y), 'xMax'=max(data.xy$y))

  df <- data.xy[, 'y']
  dt <- binProportion(df,'y', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.rds")
  
  df <- data.xy[, c('y','group')]
  dt <- binProportion(df, 'y','group', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.rds")

  df <- data.xy[, c('y','panel')]
  dt <- binProportion(df, 'y', NULL, 'panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.panel.rds")

  df <- data.xy[, c('y','group','panel')]
  dt <- binProportion(df, 'y', 'group', 'panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.panel.rds")  
})

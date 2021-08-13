context('bin')

test_that("binProportion() returns an appropriately sized data.table", {
  viewport <- list('xMin'=min(data.xy$entity.y), 'xMax'=max(data.xy$entity.y)) 
  df <- data.xy[, 'entity.y']

  dt <- binProportion(df,'entity.y', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(df,'entity.y', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)

  df <- data.xy[, c('entity.y','entity.group')]
  dt <- binProportion(df, 'entity.y','entity.group', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.y','entity.group', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt$binLabel), 'value'=unlist(dt$value))
  expect_true(all(dt_inter[, sum(value), by='binLabel'][['V1']] == 1))

  df <- data.xy[, c('entity.y','entity.panel')]
  dt <- binProportion(df, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$entity.panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))

  df <- data.xy[, c('entity.y','entity.group','entity.panel')]
  dt <- binProportion(df, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt[dt$entity.panel == 'panel1']$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'binLabel','value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$entity.panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))
  
})

test_that("binProportion() returns consistent results", {
  viewport = list('xMin'=min(data.xy$entity.y), 'xMax'=max(data.xy$entity.y))

  df <- data.xy[, 'entity.y']
  dt <- binProportion(df,'entity.y', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.rds")
  
  df <- data.xy[, c('entity.y','entity.group')]
  dt <- binProportion(df, 'entity.y','entity.group', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.rds")

  df <- data.xy[, c('entity.y','entity.panel')]
  dt <- binProportion(df, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.panel.rds")

  df <- data.xy[, c('entity.y','entity.group','entity.panel')]
  dt <- binProportion(df, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.panel.rds")  
})

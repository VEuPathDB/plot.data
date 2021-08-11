context('bin')

test_that("binProportion() returns an appropriately sized data.table", {
  viewport <- list('xMin'=min(data.xy$entity.y), 'xMax'=max(data.xy$entity.y)) 

  dt <- binProportion(data.xy,'entity.y', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(data.xy,'entity.y', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(sum(dt$value[[1]]), 1)

  dt <- binProportion(data.xy, 'entity.y','entity.group', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(data.xy, 'entity.y','entity.group', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'binLabel', 'binStart', 'binEnd', 'value'))
  dt_inter <- data.table('binLabel'=unlist(dt$binLabel), 'value'=unlist(dt$value))
  expect_true(all(dt_inter[, sum(value), by='binLabel'][['V1']] == 1))

  dt <- binProportion(data.xy, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(data.xy, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$entity.panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))

  dt <- binProportion(data.xy, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(unlist(lapply(dt[dt$entity.panel == 'panel1']$value, sum)) == 1), TRUE)
  
  dt <- binProportion(data.xy, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.panel == 'panel1']$binLabel), 'value'=unlist(dt[dt$entity.panel == 'panel1']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))
  

})

test_that("binProportion() returns consistent results", {
  viewport = list('xMin'=min(data.xy$entity.y), 'xMax'=max(data.xy$entity.y))

  dt <- binProportion(data.xy,'entity.y', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.rds")
  dt <- binProportion(data.xy, 'entity.y','entity.group', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.rds")
  dt <- binProportion(data.xy, 'entity.y', NULL, 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.panel.rds")
  dt <- binProportion(data.xy, 'entity.y', 'entity.group', 'entity.panel', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_equal_to_reference(dt,"proportion.group.panel.rds")  
})

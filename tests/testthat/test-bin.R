context('bin')

test_that("binProportion() returns an appropriately sized data.table", {
  viewport <- list('xMin'=min(testDF$entity.contB), 'xMax'=max(testDF$entity.contB))
  testDF <- as.data.table(testDF)
  df <- testDF[, 'entity.contB']

  dt <- binProportion(df,'entity.contB', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(data.table('const' = c(1, 1, 1, 1, 1, 1, 1)),'const', barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(df,'entity.contB', barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(df,'entity.contB', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)

  dt <- binProportion(df,'entity.contB', barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)

  df <- testDF[, c('entity.contB','entity.cat3')]
  dt <- binProportion(df, 'entity.contB','entity.cat3', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.contB','entity.cat3', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt$binLabel), 'value'=unlist(dt$value))
  expect_true(all(unlist(lapply(dt_inter[, sum(value), by='binLabel'][['V1']], function(x) all.equal(x,1)))))

  df <- testDF[, c('entity.contB','entity.cat4')]
  dt <- binProportion(df, 'entity.contB', NULL, 'entity.cat4', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.contB', NULL, 'entity.cat4', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.cat4 == 'cat4_a']$binLabel), 'value'=unlist(dt[dt$entity.cat4 == 'cat4_a']$value))
  expect_true(all(dt_inter[, sum(value), by=c('binLabel')][['V1']] == 1))

  df <- testDF[, c('entity.contB','entity.cat3','entity.cat4')]
  dt <- binProportion(df, 'entity.contB', 'entity.cat3', 'entity.cat4', binWidth=.1, barmode = 'overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(all(unlist(lapply(dt[dt$entity.cat4 == 'panel1']$value, sum)) == 1), TRUE)
  
  dt <- binProportion(df, 'entity.contB', 'entity.cat3', 'entity.cat4', binWidth=.1, barmode = 'stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'binLabel','value', 'binStart', 'binEnd'))
  dt_inter <- data.table('binLabel'=unlist(dt[dt$entity.cat4 == 'cat4_a']$binLabel), 'value'=unlist(dt[dt$entity.cat4 == 'cat4_a']$value))
  expect_true(all(unlist(lapply(dt_inter[, sum(value), by='binLabel'][['V1']], function(x) all.equal(x,1)))))
  
})


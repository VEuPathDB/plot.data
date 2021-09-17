context('bin')

test_that("binProportion() returns an appropriately sized data.table", {
  viewport <- list('xMin'=min(test.df$entity.contB), 'xMax'=max(test.df$entity.contB))
  test.df <- as.data.table(test.df)
  df <- test.df[, 'entity.contB']

  dt <- binProportion(df,'entity.contB', binWidth=.1, barmode='overlay', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)
  
  dt <- binProportion(df,'entity.contB', binWidth=.1, barmode='stack', viewport=viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('binLabel', 'value', 'binStart', 'binEnd'))
  expect_equal(sum(dt$value[[1]]), 1)

  df <- test.df[, c('entity.contB','entity.cat3')]
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

  df <- test.df[, c('entity.contB','entity.cat4')]
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

  df <- test.df[, c('entity.contB','entity.cat3','entity.cat4')]
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

# test_that("binProportion() returns consistent results", {
#   viewport = list('xMin'=min(test.df$entity.contB), 'xMax'=max(test.df$entity.contB))

#   df <- test.df[, 'entity.contB']
#   dt <- binProportion(df,'entity.contB', binWidth=.1, barmode = 'overlay', viewport=viewport)
#   # expect_equal_to_reference(dt,"proportion.rds")
  
#   df <- test.df[, c('entity.contB','entity.cat3')]
#   dt <- binProportion(df, 'entity.contB','entity.cat3', binWidth=.1, barmode = 'overlay', viewport=viewport)
#   # expect_equal_to_reference(dt,"proportion.group.rds")

#   df <- test.df[, c('entity.contB','entity.cat4')]
#   dt <- binProportion(df, 'entity.contB', NULL, 'entity.cat4', binWidth=.1, barmode = 'overlay', viewport=viewport)
#   # expect_equal_to_reference(dt,"proportion.panel.rds")

#   df <- test.df[, c('entity.contB','entity.cat3','entity.cat4')]
#   dt <- binProportion(df, 'entity.contB', 'entity.cat3', 'entity.cat4', binWidth=.1, barmode = 'overlay', viewport=viewport)
#   # expect_equal_to_reference(dt,"proportion.group.panel.rds")  
# })

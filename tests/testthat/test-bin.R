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

test_that("findBinSliderValues() returns integers for integer types.", {
  #for robust test, need the avg digits to not reflect a real number of digits in the sample
  x <- c(2, 1000:9999)
  xType <- 'INTEGER'

  binSlider <- findBinSliderValues.numeric(x, xType)
  expect_equal(as.numeric(binSlider$min %% 1), 0)
  expect_equal(as.numeric(binSlider$max %% 1), 0)
  expect_equal(as.numeric(binSlider$step %% 1), 0)
})

test_that("bin() adds an extra bin only when necessary due to rounding.", {

  # Force rounding up. Max of contB gets rounded up so we should get exactly 7 bins if we calculate the binWidth explicitly
  x <-  as.numeric(formatC(0 + testDF$entity.contB, digits = 3, width = 1L)) # force reasonable number of digits
  x[1] <- 3.99999
  binWidth <- (max(x) - min(x)) / 7
  xRange <- list('xMin' = min(x)
                ,'xMax' = max(x))

  bins <- bin(x, binWidth, xRange, stringsAsFactors=TRUE)
  expect_equal(length(levels(bins)), 7)

  # Force rounding down at the last bin so that the max data point needs to go in an added extra bin
  x <-  as.numeric(formatC(0 + testDF$entity.contB, digits = 3, width = 1L)) # force reasonable number of digits
  x[1] <- 3.11111
  binWidth <- (max(x) - min(x)) / 7
  xRange <- list('xMin' = min(x)
                ,'xMax' = max(x))

  bins <- bin(x, binWidth, xRange, stringsAsFactors=TRUE)
  expect_equal(length(levels(bins)), 8)


})
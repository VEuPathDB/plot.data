context('group')

#test one simple one
test_that("groupSummary() returns an appropriately sized data.table", {
  dt <- groupSummary(data.xy, NULL, 'y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('min', 'q1', 'median', 'q3', 'max'))
 
  dt <- groupSummary(data.xy, NULL, 'y', 'group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.xy, NULL, 'y', NULL, 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.xy, NULL, 'y', 'group', 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('group', 'panel', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'x', 'y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'x', 'y', 'group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'x', 'y', NULL, 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'x', 'y', 'group', 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('group', 'panel', 'x', 'min', 'q1', 'median', 'q3', 'max'))
})

#test the more complicated ones
test_that("groupSmoothedMean() returns an appropriately sized data.table", {
  dt <- groupSmoothedMean(data.xy, 'x', 'y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', 'group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'group'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', NULL, 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'panel'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', 'group', 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'group', 'panel'))
})

test_that("groupDensity() returns an appropriately sized data.table", {
  dt <- groupDensity(data.xy, 'x')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('densityX', 'densityY'))

  dt <- groupDensity(data.xy, 'y', 'group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'densityX', 'densityY'))

  dt <- groupDensity(data.xy, 'y', NULL, 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'densityX', 'densityY'))

  dt <- groupDensity(data.xy, 'y', 'group', 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('group', 'panel', 'densityX', 'densityY'))
})

#my canaries
test_that("groupSmoothedMean() returns consistent results", {
  dt <- groupSmoothedMean(data.xy, 'x', 'y')
  expect_equal_to_reference(dt,"loess.rds")
  dt <- groupSmoothedMean(data.xy, 'x', 'y','group')
  expect_equal_to_reference(dt,"loess.group.rds")
  dt <- groupSmoothedMean(data.xy, 'x', 'y', NULL, 'panel')
  expect_equal_to_reference(dt,"loess.panel.rds")
  dt <- groupSmoothedMean(data.xy, 'x', 'y', 'group', 'panel')
  expect_equal_to_reference(dt,"loess.group.panel.rds")
  dt <- groupSmoothedMean(bigData.xy, 'x', 'y')
  expect_equal_to_reference(dt,"gam.rds")
  dt <- groupSmoothedMean(bigData.xy, 'x', 'y','group')
  expect_equal_to_reference(dt,"gam.group.rds")
  dt <- groupSmoothedMean(bigData.xy, 'x', 'y', NULL, 'panel')
  expect_equal_to_reference(dt,"gam.panel.rds")
  dt <- groupSmoothedMean(bigData.xy, 'x', 'y', 'group', 'panel')
  expect_equal_to_reference(dt,"gam.group.panel.rds")
})

test_that("groupDensity() returns consistent results", {
  dt <- groupDensity(data.xy,'y')
  expect_equal_to_reference(dt,"density.rds")
  dt <- groupDensity(data.xy, 'y','group')
  expect_equal_to_reference(dt,"density.group.rds")
  dt <- groupDensity(data.xy, 'y', NULL, 'panel')
  expect_equal_to_reference(dt,"density.panel.rds")
  dt <- groupDensity(data.xy, 'y', 'group', 'panel')
  expect_equal_to_reference(dt,"density.group.panel.rds")
})

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
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', 'group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'group'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', NULL, 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'panel'))

  dt <- groupSmoothedMean(data.xy, 'x', 'y', 'group', 'panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'group', 'panel'))
})

test_that("groupDensity() returns an appropriately sized data.table", {
  dt <- groupDensity(data.xy, y='x')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('densityX', 'densityY'))

  dt <- groupDensity(data.xy, y='y', group='group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('group', 'densityX', 'densityY'))

  dt <- groupDensity(data.xy, y='y', group=NULL, panel='panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('panel', 'densityX', 'densityY'))

  dt <- groupDensity(data.xy, y='y', group='group', panel='panel')
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
  dt <- groupDensity(data.xy, x=NULL, 'y')
  expect_equal_to_reference(dt,"density.rds")
  dt <- groupDensity(data.xy, x=NULL, 'y','group')
  expect_equal_to_reference(dt,"density.group.rds")
  dt <- groupDensity(data.xy, x=NULL, 'y', group=NULL, 'panel')
  expect_equal_to_reference(dt,"density.panel.rds")
  dt <- groupDensity(data.xy, x=NULL, 'y', 'group', 'panel')
  expect_equal_to_reference(dt,"density.group.panel.rds")
})

test_that("groupProportion() returns values that sum to 1", {
  df <- data.table::data.table("labels"=c("a","a","b","b","c"), "counts"=c(1,1, 1, 1, 1), "group"=c("g1","g2","g1","g2","g1"))
  dt <- groupProportion(df, x="labels", y="counts")
  expect_equal(sum(dt$proportion[[1]]), 1)
  dt <- groupProportion(df, x="labels", y="counts", group="group")
  expect_equal(all(lapply(dt$proportion, sum) == 1), TRUE)
})

test_that("groupProportion() maps to groupSize() correctly", {
  df <- data.table::data.table("labels"=c("a","a","b","b","a"), "counts"=c(1,1, 1, 1, 1))
  dt_size <- groupSize(df, x="labels", y="counts")
  dt_prop <- groupProportion(df, x="labels", y="counts")
  expect_equal(dt_size$size[[1]]/sum(dt_size$size[[1]]), dt_prop$proportion[[1]])
})

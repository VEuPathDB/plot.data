context('group')

#test one simple one
test_that("groupSummary() returns an appropriately sized data.table", {
  dt <- groupSummary(data.xy, NULL, 'entity.y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('min', 'q1', 'median', 'q3', 'max'))
 
  dt <- groupSummary(data.xy, NULL, 'entity.y', 'entity.group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.xy, NULL, 'entity.y', NULL, 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.xy, NULL, 'entity.y', 'entity.group', 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'entity.x', 'entity.y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('entity.x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'entity.x', 'entity.y', 'entity.group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'entity.x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'entity.x', 'entity.y', NULL, 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'entity.x', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(data.catXcontY, 'entity.x', 'entity.y', 'entity.group', 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'entity.x', 'min', 'q1', 'median', 'q3', 'max'))
})

#test the more complicated ones
test_that("groupSmoothedMean() returns an appropriately sized data.table", {

  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', 'entity.group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'entity.group'))

  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', NULL, 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'entity.panel'))

  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', 'entity.group', 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'entity.group', 'entity.panel'))
})

test_that("groupDensity() returns an appropriately sized data.table", {
  
  df <- copy(data.xy)
  dt <- groupDensity(df, 'entity.x')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('densityX', 'densityY'))

  df <- copy(data.xy)
  dt <- groupDensity(df, 'entity.y', 'entity.group')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.group', 'densityX', 'densityY'))

  df <- copy(data.xy)
  dt <- groupDensity(df, 'entity.y', NULL, 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.panel', 'densityX', 'densityY'))

  df <- copy(data.xy)
  dt <- groupDensity(df, 'entity.y', 'entity.group', 'entity.panel')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt), c('entity.group', 'entity.panel', 'densityX', 'densityY'))
})

#my canaries
test_that("groupSmoothedMean() returns consistent results", {
  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y')
  expect_equal_to_reference(dt,"loess.rds")
  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y','entity.group')
  expect_equal_to_reference(dt,"loess.group.rds")
  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', NULL, 'entity.panel')
  expect_equal_to_reference(dt,"loess.panel.rds")
  df <- copy(data.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', 'entity.group', 'entity.panel')
  expect_equal_to_reference(dt,"loess.group.panel.rds")
  df <- copy(bigData.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y')
  expect_equal_to_reference(dt,"gam.rds")
  df <- copy(bigData.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y','entity.group')
  expect_equal_to_reference(dt,"gam.group.rds")
  df <- copy(bigData.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', NULL, 'entity.panel')
  expect_equal_to_reference(dt,"gam.panel.rds")
  df <- copy(bigData.xy)
  dt <- groupSmoothedMean(df, 'entity.x', 'entity.y', 'entity.group', 'entity.panel')
  expect_equal_to_reference(dt,"gam.group.panel.rds")
})

test_that("groupDensity() returns consistent results", {
  dt <- groupDensity(data.xy,'entity.y')
  expect_equal_to_reference(dt,"density.rds")
  dt <- groupDensity(data.xy, 'entity.y','entity.group')
  expect_equal_to_reference(dt,"density.group.rds")
  dt <- groupDensity(data.xy, 'entity.y', NULL, 'entity.panel')
  expect_equal_to_reference(dt,"density.panel.rds")
  dt <- groupDensity(data.xy, 'entity.y', 'entity.group', 'entity.panel')
  expect_equal_to_reference(dt,"density.group.panel.rds")
})

test_that("groupProportion() returns values that sum to 1", {
  df <- data.frame("labels"=c("a","a","b","b","c"), "counts"=c(1,1, 1, 1, 1), "group"=c("g1","g2","g1","g2","g1"))
  dt <- groupProportion(df, x="labels", y="counts")
  expect_equal(sum(dt$proportion[[1]]), 1)
  dt <- groupProportion(df, x="labels", y="counts", group="group")
  expect_equal(all(lapply(dt$proportion, sum) == 1), TRUE)
})

test_that("groupProportion() maps to groupSize() correctly", {
  df <- data.frame("labels"=c("a","a","b","b","a"), "counts"=c(1,1, 1, 1, 1))
  dt_size <- groupSize(df, x="labels", y="counts")
  dt_prop <- groupProportion(df, x="labels", y="counts")
  expect_equal(dt_size$size[[1]]/sum(dt_size$size[[1]]), dt_prop$proportion[[1]])
})

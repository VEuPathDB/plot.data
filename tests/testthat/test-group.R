context('group')

#test one simple one
test_that("groupSummary() returns an appropriately sized data.table", {
  dt <- groupSummary(test.df, NULL, 'entity.contB')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('min', 'q1', 'median', 'q3', 'max'))
 
  dt <- groupSummary(test.df, NULL, 'entity.contB', 'entity.cat3')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, NULL, 'entity.contB', NULL, 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, NULL, 'entity.contB', 'entity.cat3', 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, 'entity.cat6', 'entity.contB')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('entity.cat6', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, 'entity.cat6', 'entity.contB', 'entity.cat3')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat6', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, 'entity.cat6', 'entity.contB', NULL, 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'entity.cat6', 'min', 'q1', 'median', 'q3', 'max'))

  dt <- groupSummary(test.df, 'entity.cat6', 'entity.contB', 'entity.cat3', 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'entity.cat6', 'min', 'q1', 'median', 'q3', 'max'))
})

#test the more complicated ones
test_that("groupSmoothedMean() returns an appropriately sized data.table", {

  dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', 'entity.cat3')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', NULL, 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', 'entity.cat3', 'entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
})

test_that("groupDensity() returns an appropriately sized data.table", {
  dt <- groupDensity(test.df, y='entity.contA')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt), c('densityX', 'densityY'))

  dt <- groupDensity(test.df, y='entity.contB', group='entity.cat3')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),3)
  expect_equal(names(dt), c('entity.cat3', 'densityX', 'densityY'))

  dt <- groupDensity(test.df, y='entity.contB', group=NULL, panel='entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt), c('entity.cat4', 'densityX', 'densityY'))

  dt <- groupDensity(test.df, y='entity.contB', group='entity.cat3', panel='entity.cat4')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),12)
  expect_equal(names(dt), c('entity.cat3', 'entity.cat4', 'densityX', 'densityY'))
})

#my canaries
# test_that("groupSmoothedMean() returns consistent results", {
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB')
#   expect_equal_to_reference(dt,"loess.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB','entity.cat3')
#   expect_equal_to_reference(dt,"loess.group.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', NULL, 'entity.cat4')
#   expect_equal_to_reference(dt,"loess.panel.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', 'entity.cat3', 'entity.cat4')
#   expect_equal_to_reference(dt,"loess.group.panel.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB')
#   expect_equal_to_reference(dt,"gam.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB','entity.cat3')
#   expect_equal_to_reference(dt,"gam.group.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', NULL, 'entity.cat4')
#   expect_equal_to_reference(dt,"gam.panel.rds")
#   dt <- groupSmoothedMean(test.df, 'entity.contA', 'entity.contB', 'entity.cat3', 'entity.cat4')
#   expect_equal_to_reference(dt,"gam.group.panel.rds")
# })

# test_that("groupDensity() returns consistent results", {
#   dt <- groupDensity(test.df, x=NULL, 'entity.contB')
#   expect_equal_to_reference(dt,"density.rds")
#   dt <- groupDensity(test.df, x=NULL, 'entity.contB','entity.cat3')
#   expect_equal_to_reference(dt,"density.group.rds")
#   dt <- groupDensity(test.df, x=NULL, 'entity.contB', group=NULL, 'entity.cat4')
#   expect_equal_to_reference(dt,"density.panel.rds")
#   dt <- groupDensity(test.df, x=NULL, 'entity.contB', 'entity.cat3', 'entity.cat4')
#   expect_equal_to_reference(dt,"density.group.panel.rds")
# })

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

context('utils')

test_that("smoothed data is ordered by x", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'loess')

  expect_equal(unlist(sm$x), dt$x[order(dt$x)])
})

test_that("smoothedMean() returns a data.table", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'loess')

  expect_is(sm, 'data.table')
})

test_that("bin() does not return NA", {
  expect_false(any(is.na(bin(rnorm(100),binWidth=.1))))
})

test_that("epitabToDT() returns appropriately sized data.table", {
  m <- epitools::epitab(as.factor(rnorm(10)), as.factor(rep(c(1,2),5)))$tab
  dt <- epitabToDT(m, 'oddsratio')

  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),10)
})

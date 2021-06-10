context('utils')

test_that("smoothedMean passes along error messages for method `gam` w few values", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'gam')

  expect_equal(as.character(sm$smoothedMeanError), "Error in smooth.construct.cr.smooth.spec(object, data, knots) : \n  x has insufficient unique values to support 10 knots: reduce k.\n")
  expect_equal(class(sm$smoothedMeanError), c('scalar','character'))
  expect_equal(class(unlist(sm$smoothedMeanX)), 'numeric')
})

test_that("smoothedMean returns correct types", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'loess')

  expect_equal(class(unlist(sm$smoothedMeanX)),'character')
  expect_equal(class(unlist(sm$smoothedMeanY)),'numeric')
  expect_equal(class(unlist(sm$smoothedMeanSE)),'numeric')
})

test_that("bestFitLine returns correct types", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  bfl <- bestFitLine(dt)

  expect_equal(class(unlist(bfl$bestFitLineX)),'character')
  expect_equal(class(unlist(bfl$bestFitLineY)),'numeric')
  expect_equal(class(unlist(bfl$r2)),'numeric')
})

test_that("smoothed data is ordered by x", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'loess')

  expect_equal(as.numeric(unlist(sm$smoothedMeanX)), dt$x[order(dt$x)])
})

test_that("smoothedMean() returns a data.table", {
  dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)
  sm <- smoothedMean(dt, 'loess')

  expect_is(sm, 'data.table')
})

test_that("bin() does not return NA", {
  expect_false(any(is.na(bin(rnorm(100),binWidth=.1, viewport=list('xMin'=-5,'xMax'=5)))))
  expect_false(any(is.na(bin(rnorm(100,10),binWidth=.1, viewport=list('xMin'=5,'xMax'=15)))))
  expect_false(any(is.na(bin(rnorm(100,-10),binWidth=.1, viewport=list('xMin'=-15,'xMax'=-5)))))
})

test_that("relativeRisk() returns the right columns", {
  data <- data.table('x' = as.factor(rnorm(10)), 'y' = as.factor(rep(c(1,2),5)))
  tbl <- tableXY(data)
  dt <- relativeRisk(tbl)

  expect_equal(names(dt), c('relativerisk', 'rrInterval', 'pvalue'))
})

test_that("oddsRatio() returns the right columns", {
  data <- data.table('x' = as.factor(rnorm(10)), 'y' = as.factor(rep(c(1,2),5)))
  tbl <- tableXY(data)
  dt <- oddsRatio(tbl)

  expect_equal(names(dt), c('oddsratio', 'orInterval', 'pvalue'))
})


test_that("getAggStr() is sane", {
  expect_equal(getAggStr(NULL, NULL), ".")
  expect_equal(getAggStr(NULL, 'group'), ". ~ group")
  expect_equal(getAggStr('x', NULL), "x")
  expect_equal(getAggStr('x', 'group'), "x ~ group")
  expect_equal(getAggStr(c('x','y'), NULL), "x + y")
  expect_equal(getAggStr(NULL, c('group','panel')), ". ~ group + panel")
  expect_equal(getAggStr(c('x','y'), c('group','panel')), "x + y ~ group + panel")
})

test_that("makePanels() returns 2 entry list: 1) data.frame 2) character vector", {
  panels <- makePanels(data, 'group', 'panel')

  expect_is(panels, 'list')
  expect_equal(length(panels),2)
  expect_is(panels[[1]], 'data.frame')
  expect_equal(nrow(data), nrow(panels[[1]]))
  expect_is(panels[[2]], 'character')
})

test_that("makePanels() does nothing if there are no facets", {
  panels <- makePanels(data)

  expect_equal(data, panels[[1]])
  expect_true(is.null(panels[[2]]))
})

test_that("contingencyDT() returns appropriately sized data.table", {
  dt <- contingencyDT(data.binned)

  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),data.table::uniqueN(data.binned$x))
  expect_equal(length(dt),data.table::uniqueN(data.binned$y)+1)
})

context('utils')

test_that("plotRefMapToList returns NULL for entityId when there isnt one", {
  map <- data.frame('id' = c('group', 'y', 'panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  
  expect_equal(is.null(xVariableDetails$entityId),TRUE)

  map <- data.frame('id' = c('.group', '.y', '.panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  
  expect_equal(is.null(xVariableDetails$entityId),TRUE)

  map <- data.frame('id' = c('a.group', 'b.y', 'c.panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  
  expect_equal(is.null(xVariableDetails$entityId),FALSE)
  expect_equal(xVariableDetails$entityId,'c')
})

test_that("plotRefMapToList returns displayLabel only when defined", {
  # Without a displayLabel for any var
  map <- data.frame('id' = c('group', 'y', 'panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  
  expect_equal(is.null(xVariableDetails$displayLabel), TRUE)
  
  # With a displayLabel for all vars
  map <- data.frame('id' = c('group', 'y', 'panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('var1','var2','var3'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  
  expect_equal(is.null(xVariableDetails$displayLabel), FALSE)
  expect_equal(xVariableDetails$displayLabel, 'var3')
  
  # With a displayLabel for some vars
  map <- data.frame('id' = c('group', 'y', 'panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 'displayLabel' = c('var1','','var3'), stringsAsFactors=FALSE)

  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  yVariableDetails <- plotRefMapToList(map, 'yAxisVariable')
  
  expect_equal(is.null(xVariableDetails$displayLabel), FALSE)
  expect_equal(xVariableDetails$displayLabel, 'var3')
  expect_equal(is.null(yVariableDetails$displayLabel), TRUE)
 
})

test_that("plotRefMapToNull returns naToZero only when defined", {
    
  # Set naToZero for some vars
  map <- data.frame('id' = c('group', 'y', 'panel'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'naToZero' = c('TRUE','','FALSE'), stringsAsFactors=FALSE)
  
  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  yVariableDetails <- plotRefMapToList(map, 'yAxisVariable')
  overlayVariableDetails <- plotRefMapToList(map, 'overlayVariable')
  
  expect_equal(xVariableDetails$naToZero, 'FALSE')
  expect_true(is.null(yVariableDetails$naToZero))
  expect_equal(overlayVariableDetails$naToZero, 'TRUE')


})

test_that("toColNameOrNull works", {
  varDetailsList <- list('variableId' = 'var',
                         'entityId' = 'entity',
                         'dataType' = NULL,
                         'dataShape' = NULL)
  colName <- toColNameOrNull(varDetailsList)

  expect_equal(colName, 'entity.var')

  varDetailsList <- list('variableId' = 'var',
                         'entityId' = NULL,
                         'dataType' = NULL,
                         'dataShape' = NULL)
  colName <- toColNameOrNull(varDetailsList)

  expect_equal(colName, 'var')

  varDetailsList <- list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL,
                         'dataShape' = NULL)
  colName <- toColNameOrNull(varDetailsList)

  expect_equal(is.null(colName), TRUE)

  varDetailsList <- list('variableId' = '',
                         'entityId' = '',
                         'dataType' = NULL,
                         'dataShape' = NULL)
  expect_equal(is.null(colName), TRUE)
})

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

test_that("bin() returns appropriate bins for dates", {
  viewport <- list('xMin'=min(testDF$entity.dateA), 'xMax'=max(testDF$entity.dateA))
  dt <- testDF

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), 'day', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-12-01 - 1999-12-02")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), '4 day', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-03")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), 'week', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-06")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), '2 week', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-13")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), 'month', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-12-01 - 2000-01-01")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), '2 month', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-01 - 2000-01-01")   

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), 'year', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-01-01 - 2000-01-01")

  dt$dateBin <- bin(as.Date(testDF$entity.dateA), '2 year', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-01-01 - 2001-01-01")
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
  panels <- makePanels(testDF, 'cat3', 'cat5')

  expect_is(panels, 'list')
  expect_equal(length(panels),2)
  expect_is(panels[[1]], 'data.frame')
  expect_equal(nrow(testDF), nrow(panels[[1]]))
  expect_is(panels[[2]], 'character')
})

test_that("makePanels() does nothing if there are no facets", {
  panels <- makePanels(data)

  expect_equal(data, panels[[1]])
  expect_true(is.null(panels[[2]]))
})

test_that("contingencyDT() returns appropriately sized data.table", {
  # dt <- contingencyDT(data.binned)
  dt <- contingencyDT(testDF[, c('entity.cat3','entity.cat4','entity.cat7','entity.cat6')])

  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),data.table::uniqueN(testDF$cat7))
  expect_equal(length(dt),data.table::uniqueN(testDF$cat6)+1)
})


test_that("nonparametricTest() errs gracefully", {
  df <- as.data.frame(testDF)
  result <- nonparametricTest(df$entity.contA, df$entity.cat1)
  expect_true(grepl( 'Error', result$statsError, fixed = TRUE))
  
})

test_that("nonparametricTestByGroup() errs gracefully", {
  df <- as.data.frame(testDF)
  df$entity.cat3[df$entity.cat4 == 'cat4_a'] <- 'cat3_a'
  result <- nonparametricByGroup(df, 'entity.contA', 'entity.cat3', 'entity.cat4')
  # Expect four rows but only one error
  expect_equal(nrow(result), 4)
  expect_equal(sum(rapply(result, function(x) {grepl('Error', x, fixed=TRUE)})), 1)
})

test_that("nonparametricTest() types do not change on error", {
  df <- as.data.frame(testDF)
  result_correct <- nonparametricTest(df$entity.contA, df$entity.cat3)    # kruskal.test
  result_err <- nonparametricTest(df$entity.contA, df$entity.cat1)
  expect_true(grepl( 'Error', result_err$statsError, fixed = TRUE))
  expect_equal(result_correct$statsError, jsonlite::unbox(''))
  expect_equal(lapply(result_correct, typeof), lapply(result_err, typeof))
  
  result_correct <- nonparametricTest(df$entity.contA, df$entity.binA)    # wilcox.test
  expect_equal(result_correct$statsError, jsonlite::unbox(''))
  expect_equal(lapply(result_correct, typeof), lapply(result_err, typeof))
})

test_that("findBinWidth returns appropriate bin types for numeric inputs", {
  # Integers should return integer bin width
  x <- c(1.0, 4.0, 2.0, 9.0, 4.0, 1.0, 8.0, 11.0, 34.0, 23.0, 19.0)
  binWidth <- findBinWidth(x)
  expect_true(is.numeric(binWidth))
  expect_equal(binWidth%%1, 0)
  
  x <- c(1.0, -4.0, 2.0, 9.0, 4.0, -1.0, 8.0, -11.0, 34.0, 23.0, -19.0)
  binWidth <- findBinWidth(x)
  expect_true(is.numeric(binWidth))
  expect_equal(binWidth%%1, 0)
  
  x <- sample(-100:100, 100, replace=T)
  binWidth <- findBinWidth(x)
  expect_true(is.numeric(binWidth))
  expect_equal(binWidth%%1, 0)
  
  # Values with non-zero decimals should return numeric
  x <- runif(100, min=-1, max=1)
  binWidth <- findBinWidth(x)
  expect_true(is.numeric(binWidth))
  
})

test_that("findBinWidth returns sane results", {
  x <- c(as.Date('2021-12-25'))
  expect_equal(findBinWidth(x), 'day')

  x <- c(as.Date('2021-12-25'), as.Date('2021-12-25'))
  expect_equal(findBinWidth(x), 'day')

  x <- c(1,2,3)
  expect_equal(findBinWidth(x), 1)
  
  x <- c(1)
  expect_equal(findBinWidth(x), 1)

  x <- c(2,2,2)
  expect_equal(findBinWidth(x), 1)

  x <- c(1.2345,1.6789)
  expect_equal(findBinWidth(x), 0.2222)

  x <- c(1.2345)
  expect_equal(findBinWidth(x), 0)

  x <- c(0,0,0,0,0,0,0,.123445)
  expect_equal(findBinWidth(x), .02)

  x <- c(NA)
  expect_equal(findBinWidth(x), NA)

  x <- c(NULL)
  expect_equal(findBinWidth(x), NULL)

  x <- c(1,2,3,NA)
  expect_equal(findBinWidth(x), NA)

  x <- c(1,2,3,NA)
  expect_equal(findBinWidth(x, na.rm = T), 1)
})



test_that("bin returns appropriate data with default binWidth", {
  
  # numbers
  df <- testDF[['entity.contA']]
  viewport <- list('xMin'= 0.1, 'xMax'=0.9)
  dt <- bin(df, viewport=viewport)
  expect_equal(class(dt), 'character')
  expect_equal(length(dt), 33)
  
  # dates
  df <- as.Date(testDF[['entity.dateA']])
  viewport <- list('xMin' = min(df), 'xMax' = max(df))
  dt <- bin(df, viewport=viewport)
  expect_equal(class(dt), 'character')
  expect_equal(length(df), length(dt))
})

test_that("breaks returns appropriate results", {
  
  df <- testDF[['entity.contA']]
  
  dt <- breaks(df, 'numbers', nbins=20)
  expect_equal(length(dt), 21)
  
  dt <- breaks(df, 'numbers', binwidth=0.5)
  expect_equal(length(dt), 3)
  expect_equal(names(dt), c('0%','50%','100%'))
  
  dt <- breaks(df, 'width', binwidth=0.5)
  expect_equal(length(dt), 59)
})


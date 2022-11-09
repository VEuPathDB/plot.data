context('utils')

test_that("numBinsToBinWidth() returns a binWidth that will actually provide the desired bins.", {
  binWidth <- numBinsToBinWidth(testDF$entity.int6, 8)
  viewport <- findViewport(testDF$entity.int6, 'NUMBER')
  x <- bin(testDF$entity.int6, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),5)

  binWidth <- numBinsToBinWidth(testDF$entity.int6, 1)
  viewport <- findViewport(testDF$entity.int6, 'NUMBER')
  x <- bin(testDF$entity.int6, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),1)
  
  binWidth <- numBinsToBinWidth(testDF$entity.contA, 8)
  viewport <- findViewport(testDF$entity.contA, 'NUMBER')
  x <- bin(testDF$entity.contA, binWidth, viewport)
  expect_equal(data.table::uniqueN(x), 8)

  binWidth <- numBinsToBinWidth(testDF$entity.contA, 23)
  viewport <- findViewport(testDF$entity.contA, 'NUMBER')
  x <- bin(testDF$entity.contA, binWidth, viewport)
  expect_equal(data.table::uniqueN(x), 23)

  binWidth <- numBinsToBinWidth(testDF$entity.contA, 2)
  viewport <- findViewport(testDF$entity.contA, 'NUMBER')
  x <- bin(testDF$entity.contA, binWidth, viewport)
  expect_equal(data.table::uniqueN(x), 2)

  binWidth <- numBinsToBinWidth(testDF$entity.contA, 500)
  viewport <- findViewport(testDF$entity.contA, 'NUMBER')
  x <- bin(testDF$entity.contA, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),259) # this is not ridiculous - there are just a lot of empty bins! 501 levels -> 259 non-empty bins

  date <- as.Date(testDF$entity.dateA)
  binWidth <- numBinsToBinWidth(date, 8)
  viewport <- findViewport(date, 'DATE')
  x <- bin(date, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),8)

  binWidth <- numBinsToBinWidth(date, 27)
  viewport <- findViewport(date, 'DATE')
  x <- bin(date, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),27)

  binWidth <- numBinsToBinWidth(date, 2)
  viewport <- findViewport(date, 'DATE')
  x <- bin(date, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),2)

  binWidth <- numBinsToBinWidth(date, 393)
  viewport <- findViewport(date, 'DATE')
  x <- bin(date, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),256) # another levels difference. Actually there are 363 bins (note i think cut() isn't great for dates)

  binWidth <- numBinsToBinWidth(date, 500)
  viewport <- findViewport(date, 'DATE')
  x <- bin(date, binWidth, viewport)
  expect_equal(data.table::uniqueN(x),280) # 499 bins including those with 0 counts. Difference between factors and not factors
})

test_that("chiSq returns consistent classes", {
  tbl1 <- table(data.table('Antibiotic'=c('Azithromycin'),'Disease'=c('Yes','No')))
  tbl2 <- table(data.table('Antibiotic'=c('Azithromycin','Placebo'),'Disease'=c('Yes','No')))

  expect_equal(all(unlist(lapply(chiSq(tbl1),is.numeric))), TRUE)
  expect_equal(all(unlist(lapply(chiSq(tbl2),is.numeric))), TRUE)
})

test_that("roundedGeometricMean() returns reasonable results.", {
  noValues <- numeric()
  singlePositiveValue <- rnorm(1, 10)
  negativeValues <- rnorm(1000, -10)
  positiveValues <- rnorm(1000, 10)
  ignoreValues <- c(-10, 0, positiveValues)
  onlyZeroValues <- c(0,0,0,0,0)

  expect_equal(roundedGeometricMean(negativeValues), NaN)
  expect_equal(roundedGeometricMean(noValues), NaN)
  expect_equal(roundedGeometricMean(onlyZeroValues), NaN)
  expect_warning(roundedGeometricMean(ignoreValues))
  expect_equal(roundedGeometricMean(singlePositiveValue), round(singlePositiveValue,4))
  expect_equal(roundedGeometricMean(positiveValues), roundedGeometricMean(ignoreValues))
  expect_equal(roundedGeometricMean(positiveValues) < roundedMean(positiveValues), TRUE)
})

test_that("*CI fxns return reasonable results", {
  noValues <- numeric()
  singleValue <- rnorm(1)
  fewValues <- rnorm(2)
  manyValues <- rnorm(1000)

  expect_equal(nchar(meanCI(noValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(meanCI(noValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(meanCI(noValues)[[1]]$upperBound), TRUE)

  expect_equal(nchar(meanCI(singleValue)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(meanCI(singleValue)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(meanCI(singleValue)[[1]]$upperBound), TRUE)

  expect_equal(nchar(meanCI(fewValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(meanCI(fewValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(meanCI(fewValues)[[1]]$upperBound), TRUE)
  expect_equal(meanCI(fewValues)[[1]]$lowerBound <= mean(fewValues), TRUE)
  expect_equal(meanCI(fewValues)[[1]]$upperBound >= mean(fewValues), TRUE)

  expect_equal(nchar(meanCI(manyValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(meanCI(manyValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(meanCI(manyValues)[[1]]$upperBound), TRUE)
  expect_equal(meanCI(manyValues)[[1]]$lowerBound <= mean(manyValues), TRUE)
  expect_equal(meanCI(manyValues)[[1]]$upperBound >= mean(manyValues), TRUE)
  expect_equal(meanCI(manyValues)[[1]]$lowerBound > min(manyValues), TRUE)
  expect_equal(meanCI(manyValues)[[1]]$upperBound < max(manyValues), TRUE)

  expect_equal(nchar(medianCI(noValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(medianCI(noValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(medianCI(noValues)[[1]]$upperBound), TRUE)

  expect_equal(nchar(medianCI(singleValue)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(medianCI(singleValue)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(medianCI(singleValue)[[1]]$upperBound), TRUE)

  expect_equal(nchar(medianCI(fewValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(medianCI(fewValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(medianCI(fewValues)[[1]]$upperBound), TRUE)

  expect_equal(nchar(medianCI(manyValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(medianCI(manyValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(medianCI(manyValues)[[1]]$upperBound), TRUE)
  expect_equal(medianCI(manyValues)[[1]]$lowerBound <= median(manyValues), TRUE)
  expect_equal(medianCI(manyValues)[[1]]$upperBound >= median(manyValues), TRUE)
  expect_equal(medianCI(manyValues)[[1]]$lowerBound > min(manyValues), TRUE)
  expect_equal(medianCI(manyValues)[[1]]$upperBound < max(manyValues), TRUE)

  singlePositiveValue <- rnorm(1, 10)
  fewPositiveValues <- rnorm(2, 10)
  negativeValues <- rnorm(1000, -10)
  positiveValues <- rnorm(1000, 10)
  ignoreValues <- c(-10, 0, positiveValues)
  onlyZeroValues <- c(0,0,0,0,0)

  expect_equal(nchar(geometricMeanCI(negativeValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(geometricMeanCI(negativeValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(geometricMeanCI(negativeValues)[[1]]$upperBound), TRUE)  

  expect_equal(nchar(geometricMeanCI(noValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(geometricMeanCI(noValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(geometricMeanCI(noValues)[[1]]$upperBound), TRUE)

  expect_equal(nchar(geometricMeanCI(singlePositiveValue)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(geometricMeanCI(singlePositiveValue)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(geometricMeanCI(singlePositiveValue)[[1]]$upperBound), TRUE)

  expect_equal(nchar(geometricMeanCI(onlyZeroValues)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(geometricMeanCI(onlyZeroValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(geometricMeanCI(onlyZeroValues)[[1]]$upperBound), TRUE)

  expect_equal(nchar(geometricMeanCI(fewPositiveValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(geometricMeanCI(fewPositiveValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(geometricMeanCI(fewPositiveValues)[[1]]$upperBound), TRUE)
  expect_equal(geometricMeanCI(fewPositiveValues)[[1]]$lowerBound <= roundedGeometricMean(fewPositiveValues), TRUE)
  expect_equal(geometricMeanCI(fewPositiveValues)[[1]]$upperBound >= roundedGeometricMean(fewPositiveValues), TRUE)

  expect_equal(nchar(geometricMeanCI(positiveValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(geometricMeanCI(positiveValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(geometricMeanCI(positiveValues)[[1]]$upperBound), TRUE)
  expect_equal(geometricMeanCI(positiveValues)[[1]]$lowerBound <= roundedGeometricMean(positiveValues), TRUE)
  expect_equal(geometricMeanCI(positiveValues)[[1]]$upperBound >= roundedGeometricMean(positiveValues), TRUE)
  expect_equal(geometricMeanCI(positiveValues)[[1]]$lowerBound > min(positiveValues), TRUE)
  expect_equal(geometricMeanCI(positiveValues)[[1]]$upperBound < max(positiveValues), TRUE)

  expect_equal(nchar(geometricMeanCI(ignoreValues)[[1]]$error) == 0, TRUE)
  expect_equal(is.numeric(geometricMeanCI(ignoreValues)[[1]]$lowerBound), TRUE)
  expect_equal(is.numeric(geometricMeanCI(ignoreValues)[[1]]$upperBound), TRUE)
  expect_equal(geometricMeanCI(ignoreValues)[[1]]$lowerBound <= roundedGeometricMean(ignoreValues), TRUE)
  expect_equal(geometricMeanCI(ignoreValues)[[1]]$upperBound >= roundedGeometricMean(ignoreValues), TRUE)
  expect_equal(geometricMeanCI(ignoreValues)[[1]]$lowerBound > min(ignoreValues), TRUE)
  expect_equal(geometricMeanCI(ignoreValues)[[1]]$upperBound < max(ignoreValues), TRUE)
  expect_warning(geometricMeanCI(ignoreValues))
  expect_equal(identical(geometricMeanCI(ignoreValues), geometricMeanCI(positiveValues)), TRUE)

  noNum <- numeric()
  noDenom <- numeric()
  zeroDenom <- c(0)
  zeroNum <- c(0)
  smallNum <- c(2)
  smallDenom <- c(3)
  largeNum <- c(200)
  largeDenom <- c(1000)

  expect_equal(nchar(proportionCI(noNum, noDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(noNum, noDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(noNum, noDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(noNum, largeDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(noNum, largeDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(noNum, largeDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(largeNum, noDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(largeNum, noDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(largeNum, noDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(zeroNum, noDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(zeroNum, noDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(zeroNum, noDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(noNum,zeroDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(noNum, zeroDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(noNum, zeroDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(largeNum, zeroDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(largeNum, zeroDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(largeNum, zeroDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(zeroNum, zeroDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(zeroNum, zeroDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(zeroNum, zeroDenom)[[1]]$upperBound), TRUE)
  expect_equal(nchar(proportionCI(largeNum, smallDenom)[[1]]$error) > 0, TRUE)
  expect_equal(is.na(proportionCI(largeNum, smallDenom)[[1]]$lowerBound), TRUE)
  expect_equal(is.na(proportionCI(largeNum, smallDenom)[[1]]$upperBound), TRUE)

  expect_equal(nchar(proportionCI(zeroNum,largeDenom)[[1]]$error) == 0, TRUE)
  expect_equal(proportionCI(zeroNum, largeDenom)[[1]]$lowerBound == 0, TRUE)
  expect_equal(proportionCI(zeroNum, largeDenom)[[1]]$upperBound == 0, TRUE)

  expect_equal(nchar(proportionCI(smallNum,smallNum)[[1]]$error) == 0, TRUE)
  expect_equal(proportionCI(smallNum, smallNum)[[1]]$lowerBound == 1, TRUE)
  expect_equal(proportionCI(smallNum, smallNum)[[1]]$upperBound == 1, TRUE)

  expect_equal(nchar(proportionCI(smallNum,smallDenom)[[1]]$error) == 0, TRUE)
  expect_equal(proportionCI(smallNum, smallDenom)[[1]]$lowerBound <= smallNum/smallDenom, TRUE)
  expect_equal(proportionCI(smallNum, smallDenom)[[1]]$lowerBound >= 0, TRUE)
  expect_equal(proportionCI(smallNum, smallDenom)[[1]]$upperBound >= smallNum/smallDenom, TRUE)
  expect_equal(proportionCI(smallNum, smallDenom)[[1]]$upperBound <= 1, TRUE)

  expect_equal(nchar(proportionCI(smallNum,largeDenom)[[1]]$error) == 0, TRUE)
  expect_equal(proportionCI(smallNum, largeDenom)[[1]]$lowerBound <= smallNum/largeDenom, TRUE)
  expect_equal(proportionCI(smallNum, largeDenom)[[1]]$lowerBound >= 0, TRUE)
  expect_equal(proportionCI(smallNum, largeDenom)[[1]]$upperBound >= smallNum/largeDenom, TRUE)
  expect_equal(proportionCI(smallNum, largeDenom)[[1]]$upperBound <= 1, TRUE)

  expect_equal(nchar(proportionCI(largeNum,largeDenom)[[1]]$error) == 0, TRUE)
  expect_equal(proportionCI(largeNum, largeDenom)[[1]]$lowerBound <= largeNum/largeDenom, TRUE)
  expect_equal(proportionCI(largeNum, largeDenom)[[1]]$lowerBound >= 0, TRUE)
  expect_equal(proportionCI(largeNum, largeDenom)[[1]]$upperBound >= largeNum/largeDenom, TRUE)
  expect_equal(proportionCI(largeNum, largeDenom)[[1]]$upperBound <= 1, TRUE)
})

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

test_that("plotRefMapToNull handles many naToZero inputs", {
    
  # Set naToZero for some vars
  map <- data.frame('id' = c('group', 'y', 'panel', 'x', 'z', 'w'),
                    'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2', 'facetVariable3'),
                    'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING', 'STRING', 'STRING'),
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'),
                    'naToZero' = c('TRUE','','FALSE', NA, TRUE, FALSE), stringsAsFactors=FALSE)
  
  xVariableDetails <- plotRefMapToList(map, 'xAxisVariable')
  yVariableDetails <- plotRefMapToList(map, 'yAxisVariable')
  overlayVariableDetails <- plotRefMapToList(map, 'overlayVariable')
  facetVariableDetails1 <- plotRefMapToList(map, 'facetVariable1')
  facetVariableDetails2 <- plotRefMapToList(map, 'facetVariable2')
  facetVariableDetails3 <- plotRefMapToList(map, 'facetVariable3')
  
  expect_equal(xVariableDetails$naToZero, FALSE)
  expect_equal(yVariableDetails$naToZero, FALSE)
  expect_equal(overlayVariableDetails$naToZero, TRUE)
  expect_equal(facetVariableDetails1$naToZero, FALSE)
  expect_equal(facetVariableDetails2$naToZero, TRUE)
  expect_equal(facetVariableDetails3$naToZero, FALSE)


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

  dt$dateBin <- bin(as.Date(dt$entity.dateA), 'day', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-12-01 - 1999-12-02")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), '4 day', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-03")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), 'week', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-06")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), '2 week', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-29 - 1999-12-13")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), 'month', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-12-01 - 2000-01-01")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), '2 month', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-11-01 - 2000-01-01")   

  dt$dateBin <- bin(as.Date(dt$entity.dateA), 'year', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-01-01 - 2000-01-01")

  dt$dateBin <- bin(as.Date(dt$entity.dateA), '2 year', viewport)
  expect_equal(unique(dt$dateBin[dt$entity.dateA == '1999-12-01']), "1999-01-01 - 2001-01-01")
})

test_that("bin() returns complete list of possible bins as levels when asked for a factor.", {
  viewport <- list('xMin'=min(testDF$entity.dateA), 'xMax'=max(testDF$entity.dateA))
  dt <- testDF

  dt$dateBin <- bin(as.Date(dt$entity.dateA), 'day', viewport, stringsAsFactors=T)
  expect_equal(uniqueN(dt$dateBin), 393)
  expect_equal(uniqueN(levels(dt$dateBin)), 3986)

  viewport <- list('xMin'=min(dt$entity.contA), 'xMax'=max(dt$entity.contA))
  binWidth <- plot.data::findBinWidth(dt$entity.contA) 
  dt$contBin <- bin(dt$entity.contA, binWidth, viewport, stringsAsFactors=T)
  expect_equal(uniqueN(dt$contBin), 16)
  expect_equal(uniqueN(levels(dt$contBin)), 16)

  expandedViewport <- list('xMin'=-20, 'xMax'=20)
  dt$contBin <- bin(dt$entity.contA, binWidth, expandedViewport, stringsAsFactors=T)
  expect_equal(uniqueN(dt$contBin), 17)
  expect_equal(uniqueN(levels(dt$contBin)), 23)

  viewport <- list('xMin'=min(dt$entity.int6), 'xMax'=max(dt$entity.int6))
  binWidth <- 0
  dt$intBin <- bin(dt$entity.int6, binWidth, viewport, stringsAsFactors=T)
  expect_equal(uniqueN(dt$intBin), 6)
  expect_equal(uniqueN(levels(dt$intBin)), 6)

  expandedViewport <- list('xMin'=1, 'xMax'=7)
  dt$intBin <- bin(dt$entity.int6, binWidth, expandedViewport, stringsAsFactors=T)
  expect_equal(uniqueN(dt$intBin), 6)
  expect_equal(uniqueN(levels(dt$intBin)), 7)

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
  expect_equal(findBinWidth(x), 0.01)

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

test_that("signifDigitEpsilon returns appropriate results", {
  expect_equal(signifDigitEpsilon(1.23, 3), 0.01)
  expect_equal(signifDigitEpsilon(11.0, 3), 0.1)
  expect_equal(signifDigitEpsilon(12.3, 3), 0.1)
  expect_equal(signifDigitEpsilon(101000, 3), 1000)
  expect_equal(signifDigitEpsilon(1.20e-05, 3), 1.0e-07)
  expect_equal(signifDigitEpsilon(0.0123e-05, 3), 1.0e-09)
  expect_equal(signifDigitEpsilon(-2.34e-02, 3), 1.0e-04)
  expect_equal(signifDigitEpsilon(1234567, 7), 1)
  expect_equal(signifDigitEpsilon(-1234567, 7), 1)
})

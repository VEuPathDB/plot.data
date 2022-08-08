#
# will move this to test-histogram.R when finished with this issue/PR
#

test_that("histogram.dt does not produce corrupted bins when given TAC data from issue 182", {
  map <- data.frame('id' = c('values'),
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('NUMBER'),
                    'dataShape' = c('CONTINUOUS'),
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(data.issue182, map, binWidth = 1, value = 'count', barmode = 'stack', binReportValue = 'binWidth')

  # check that there are no NAs (which the client is getting as nulls) in the binLabels
  expect_equal(sum(is.na(unlist(dt$binLabel))), 0)
  # check that the binStart and binEnd strings are all valid numbers
  # (the bug gives values like '24]')
  expect_equal(sum(is.na(as.numeric(unlist(dt$binStart)))), 0)
  expect_equal(sum(is.na(as.numeric(unlist(dt$binEnd)))), 0)
})

test_that("histogram.dt does something XXXX TBC", {
  # Force rounding down at the last bin so that the max data point needs to go in an added extra bin
  x <-  as.numeric(formatC(0 + testDF$entity.contB, digits = 3, width = 1L)) # force reasonable number of digits
  x[1] <- 3.11111
  binWidth <- (max(x) - min(x)) / 7
  xRange <- list('xMin' = min(x)
                ,'xMax' = max(x))

  
  map <- data.frame('id' = c('values'),
                    'plotRef' = c('xAxisVariable'),
                    'dataType' = c('NUMBER'),
                    'dataShape' = c('CONTINUOUS'),
                    stringsAsFactors = FALSE)

  df <- data.frame(values=x)

  dt <- histogram.dt(df, map, binWidth = binWidth, value = 'count', barmode = 'stack', binReportValue = 'binWidth')

  expect_true((uniqueN(unlist(dt$binLabel)) - 7) %in% c(0,1,2))
})

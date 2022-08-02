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

})


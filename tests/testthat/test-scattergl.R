context('scattergl')

test_that("scattergl.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'contVar', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'DATE', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.dates

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'seriesX', 'seriesY'))
  expect_equal(as.character(range(df$date)), range(dt$seriesX))
  expect_equal(class(unlist(dt$seriesX)),'character')
  expect_equal(class(unlist(dt$seriesY)),'character')

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'group', 'panel'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))


  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'seriesX', 'seriesY'))
  numericSeriesX <- lapply(dt$seriesX, as.numeric)
  expect_equal(range(df$x), range(numericSeriesX))
  expect_equal(class(unlist(dt$seriesX)),'character')
  expect_equal(class(unlist(dt$seriesY)),'character')

  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'group', 'panel'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))

  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
  
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'densityX', 'densityY'))

  map <- data.frame('id' = c('group', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'group'))
 
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))
 
  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'densityX', 'densityY'))


  map <- data.frame('id' = c('y', 'x', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError', 'panel'))
  
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'densityX', 'densityY'))
  

  map <- data.frame('id' = c('y', 'x'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS'), stringsAsFactors = FALSE)

  dt <- scattergl.dt(df, map, 'raw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY'))

  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
  
  dt <- scattergl.dt(df, map, 'smoothedMean')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('smoothedMeanX', 'smoothedMeanY', 'smoothedMeanSE', 'smoothedMeanError'))
    
  dt <- scattergl.dt(df, map, 'bestFitLineWithRaw')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('seriesX', 'seriesY', 'bestFitLineX', 'bestFitLineY', 'r2'))

  dt <- scattergl.dt(df, map, 'density')
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('densityX', 'densityY'))
  
  # Continuous overlay
  map <- data.frame('id' = c('z', 'y', 'x'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- data.xy
  df[, z := runif(500)]
  
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c('seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_true(identical(dt$seriesGradientColorscale[[1]], df$z))
  
  map <- data.frame('id' = c('group', 'y', 'x', 'panel', 'z'), 'plotRef' = c('facetVariable2', 'yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 16)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))
  
  map <- data.frame('id' = c('y', 'x', 'panel', 'z'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'STRING', 'NUMBER'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  dt <- scattergl.dt(df, map, 'raw')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt), c('panel', 'seriesX', 'seriesY', 'seriesGradientColorscale'))
  expect_equal(length(dt$seriesGradientColorscale[[1]]), length(dt$seriesX[[1]]))
  
  
})

test_that("scattergl() returns appropriately formatted json", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  dt <- scattergl.dt(df, map, 'smoothedMeanWithRaw')
  outJson <- getJSON(dt)
  jsonList <- jsonlite::fromJSON(outJson)

  expect_equal(names(jsonList),c('scatterplot','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$scatterplot),c('data','config'))
  expect_equal(names(jsonList$scatterplot$data),c('facetVariableDetails','overlayVariableDetails','seriesX','seriesY','smoothedMeanX','smoothedMeanY','smoothedMeanSE','smoothedMeanError'))
  expect_equal(names(jsonList$scatterplot$data$facetVariableDetails),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$scatterplot$data$facetVariableDetails), 16)
  expect_equal(names(jsonList$scatterplot$config),c('incompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$scatterplot$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails','facetVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable),c('variableDetails','completeCases'))
})




test_that("scattergl.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'y', 'x', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  # Add 10 missing values to each column
  df$x[sample(1:100, 10, replace=F)] <- NA
  df$y[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  
  dt <- scattergl.dt(df, map, 'raw')
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of incompleteCases should be <= sum of incomplete cases within each var
  expect_equal(attr(dt, 'incompleteCases')[1] <= sum(nrow(df) - completecasestable$completeCases), TRUE) 
})

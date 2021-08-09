context('histogram')

test_that("histogram.dt() returns requested numBins/ binWidth", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, map, binWidth = .3, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(round(as.numeric(binSpec(dt)$value),1), .3)
  expect_equal(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))), .3)

  dt <- histogram.dt(df, map, binWidth = 1.5, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(round(as.numeric(binSpec(dt)$value),1), 1.5)
  expect_equal(round(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))),1), 1.5)

  binReportValue <- 'numBins'
  binWidth <- numBinsToBinWidth(df$var, 5)
   dt <- histogram.dt(df, map, binWidth = binWidth, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(as.numeric(binSpec(dt)$value), 5)
  expect_equal(round(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))),4), round(binWidth,4))

  binReportValue <- 'numBins'
  binWidth <- numBinsToBinWidth(df$var, 15)
   dt <- histogram.dt(df, map, binWidth = binWidth, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_equal(as.numeric(binSpec(dt)$value), 15)
  #fairly low precision bc its lost w the conversion numBins -> binWidth and back
  expect_equal(round(getMode(as.numeric(unlist(dt$binEnd)) - as.numeric(unlist(dt$binStart))),2), round(binWidth,2))
})

test_that("histogram.dt() returns a valid plot.data histogram object", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','facetVariable1', 'facetVariable2', 'summary', 'viewport', 'binSlider', 'binSpec'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','size'))
  expect_equal(nrow(sampleSizes), 16)
  expect_equal(names(namedAttrList$summary), c('min','q1','median','mean','q3','max'))
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
  expect_equal(names(namedAttrList$binSpec), c('type','value')) 
  expect_equal(round(as.numeric(namedAttrList$binSpec$value),4), 0.2681)
  expect_equal(as.character(namedAttrList$binSpec$type), 'binWidth')
  
  
  map <- data.frame('id' = c('group', 'date', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(data.dates)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, map, binWidth = 'month', value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'histogram')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','facetVariable1', 'facetVariable2', 'summary', 'viewport', 'binSlider', 'binSpec'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('panel','size'))
  expect_equal(nrow(sampleSizes), 16)
  expect_equal(names(namedAttrList$summary), c('min','q1','median','mean','q3','max'))
  expect_equal(names(viewport(dt)), c('xMin','xMax'))
  expect_equal(names(binSlider(dt)), c('min','max','step'))
  expect_equal(names(namedAttrList$binSpec), c('type','value','units'))
  expect_equal(as.numeric(namedAttrList$binSpec$value),1)
  expect_equal(as.character(namedAttrList$binSpec$type),'binWidth')
  expect_equal(as.character(namedAttrList$binSpec$unit),'month')
  expect_true(all(grepl('T00:00:00', unlist(dt$binStart))))
  expect_true(all(grepl('T00:00:00', unlist(dt$binEnd))))
  expect_true(!any(grepl('T00:00:00', unlist(dt$binLabel))))
})

test_that("histogram.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
 
  expect_equal(class(unlist(dt$panel)), 'character') 
  expect_equal(class(unlist(dt$binLabel)), 'character') 
  expect_equal(class(unlist(dt$binStart)), 'character') 
  expect_equal(class(unlist(dt$binEnd)), 'character') 
  expect_equal(class(unlist(dt$value)), 'integer') 
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSpec$value),c('scalar', 'numeric'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
  
  map <- data.frame('id' = c('group', 'date', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(data.dates)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'
  
  dt <- histogram.dt(df, map, binWidth = 'month', value='count', barmode = 'overlay', binReportValue, viewport)

  expect_equal(class(unlist(dt$panel)), 'character')
  expect_equal(class(unlist(dt$binLabel)), 'character')
  expect_equal(class(unlist(dt$binStart)), 'character')
  expect_equal(class(unlist(dt$binEnd)), 'character')
  expect_equal(class(unlist(dt$value)), 'integer')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  expect_equal(class(namedAttrList$summary$min),c('scalar', 'character'))
  expect_equal(class(namedAttrList$viewport$xMin),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSlider$min),c('scalar', 'numeric'))
  expect_equal(class(namedAttrList$binSpec$type),c('scalar', 'character'))
  expect_equal(class(namedAttrList$binSpec$value),c('scalar', 'numeric'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("histogram.dt() returns bins according to specified viewport", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  binReportValue <- 'binWidth'
  viewport <- list('xMin'=-1.5,'xMax'=2.5)
  
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  maxBinStart <- as.numeric(max(unlist(lapply(dt$binStart, max))))
  expect_true(maxBinStart <= viewport$xMax)
  minBinEnd <- as.numeric(min(unlist(lapply(dt$binEnd, min))))
  expect_true(minBinEnd >= viewport$xMin)
  
  #figure how to test for expanding to viewport, since we dont explicitly return 0 value bins..
})

test_that("histogram.dt() returns an appropriately sized data.table", {
  df <- as.data.frame(bigData)
  
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport = viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport = viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)


  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('overlayVariable', 
                                  'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('group', 'var'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable'), 
                    'dataType' = c('STRING', 'NUMBER'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('var', 'panel'), 
                    'plotRef' = c('xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('NUMBER', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('var'), 
                    'plotRef' = c('xAxisVariable'), 
                    'dataType' = c('NUMBER'), 
                    'dataShape' = c('CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))


  #this for dates
  map <- data.frame('id' = c('group', 'date', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(data.dates)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  expect_equal(all(grepl('.||.', dt$panel, fixed=T)), TRUE)


  map <- data.frame('id' = c('group', 'date', 'panel'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'DATE', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),16)
  expect_equal(names(dt),c('group', 'panel', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('group', 'date'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable'), 
                    'dataType' = c('STRING', 'DATE'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'binLabel', 'binStart', 'binEnd', 'value'))


  map <- data.frame('id' = c('date', 'panel'), 
                    'plotRef' = c('xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('DATE', 'STRING'), 
                    'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('panel', 'binLabel', 'binStart', 'binEnd', 'value'))

  map <- data.frame('id' = c('date'), 
                    'plotRef' = c('xAxisVariable'), 
                    'dataType' = c('DATE'), 
                    'dataShape' = c('CONTINUOUS'), 
                    stringsAsFactors = FALSE)

  dt <- histogram.dt(df, map, binWidth=NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'overlay', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))
  
  dt <- histogram.dt(df, map, binWidth=NULL, value='proportion', barmode = 'stack', binReportValue, viewport)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('binLabel', 'binStart', 'binEnd', 'value'))

})

test_that("histogram() returns appropriately formatted json", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable', 'completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('overlayVariableDetails','facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$overlayVariableDetails),c('variableId','entityId','value'))
  expect_equal(names(jsonList$histogram$config),c('completeCases','plottedIncompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))  
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('overlayVariableDetails', 'facetVariableDetails', 'size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(class(jsonList$sampleSizeTable$overlayVariableDetails$value[[1]]), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId')) 

  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'

  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(names(jsonList$histogram$config),c('completeCases','plottedIncompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId')) 
 
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(names(jsonList$histogram$config),c('completeCases','plottedIncompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId'))


  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('facet2Label','xLabel','facet1Label'),
                    stringsAsFactors=FALSE)
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList),c('histogram','sampleSizeTable','completeCasesTable'))
  expect_equal(names(jsonList$histogram),c('data','config'))
  expect_equal(names(jsonList$histogram$data),c('facetVariableDetails','binLabel','binStart','binEnd','value'))
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(nrow(jsonList$histogram$data$facetVariableDetails[[1]]), 2)
  expect_equal(names(jsonList$histogram$config),c('completeCases','plottedIncompleteCases','summary','viewport','binSlider','binSpec','xVariableDetails'))
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId','displayLabel'))
  expect_equal(names(jsonList$histogram$config$viewport),c('xMin','xMax'))
  expect_equal(names(jsonList$histogram$config$binSlider),c('min','max','step'))
  expect_equal(names(jsonList$histogram$config$summary),c('min','q1','median','mean','q3','max'))
  expect_equal(names(jsonList$sampleSizeTable),c('facetVariableDetails','size'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails', 'completeCases'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId', 'displayLabel'))

  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'),
                    'displayLabel' = c('','','facet1Label'),
                    stringsAsFactors=FALSE)
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'stack', binReportValue, viewport)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList$histogram$data$facetVariableDetails[[1]]),c('variableId','entityId','value', 'displayLabel'))
  expect_equal(names(jsonList$histogram$config$xVariableDetails),c('variableId','entityId'))
  expect_equal(names(jsonList$completeCasesTable$variableDetails), c('variableId', 'entityId', 'displayLabel'))
  expect_equal(class(jsonList$sampleSizeTable$facetVariableDetails[[1]]$value), 'character')
  
})

test_that("histogram.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'var', 'panel'), 
                    'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 
                    'dataType' = c('STRING', 'NUMBER', 'STRING'), 
                    'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), 
                    stringsAsFactors=FALSE)
  df <- as.data.frame(bigData)
  viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
  binReportValue <- 'binWidth'
  
  # Add 10 missing values to each column
  df$var[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', barmode = 'overlay', binReportValue, viewport)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'incompleteCases')[1] <= completecasestable$completeCases), TRUE) 
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], 0)
  dt <- histogram.dt(df, map, binWidth = NULL, value='count', binReportValue = binReportValue, viewport = viewport, evilMode = TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], sum((is.na(df$group) | is.na(df$panel)) & !is.na(df$var)))
})

context('box')

test_that("box.dt() returns a valid plot.data box object", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('group','panel','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('panel','statistics'))
  
  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'plot.data')
  expect_is(dt, 'boxplot')
  expect_equal(names(namedAttrList),c('xAxisVariable', 'yAxisVariable', 'completeCases','plottedIncompleteCases','completeCasesTable','sampleSizeTable','overlayVariable', 'statsTable'))
  completeCases <- completeCasesTable(dt)
  expect_equal(names(completeCases), c('variableDetails','completeCases'))
  expect_equal(nrow(completeCases), 3)
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(names(sampleSizes), c('group','panel','size'))
  expect_equal(nrow(sampleSizes), 4)
  expect_equal(names(namedAttrList$statsTable), c('panel','statistics'))
  expect_equal(dt$group[[1]], 'group1')
  expect_equal(dt$label[[1]], c('panel1','panel2','panel3','panel4'))
  expect_equal(unlist(lapply(dt$rawData[[1]], length)), c(25,50,25,25))
})

test_that("box.dt() returns plot data and config of the appropriate types", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE)
  expect_equal(class(unlist(dt$min)), 'numeric')
  expect_equal(class(unlist(dt$q1)), 'numeric')
  expect_equal(class(unlist(dt$median)), 'numeric')
  expect_equal(class(unlist(dt$q3)), 'numeric')
  expect_equal(class(unlist(dt$max)), 'numeric')
  expect_equal(class(unlist(dt$lowerfence)), 'numeric')
  expect_equal(class(unlist(dt$upperfence)), 'numeric')
  namedAttrList <- getPDAttributes(dt)
  expect_equal(class(namedAttrList$completeCases),c('scalar', 'integer'))
  completeCases <- completeCasesTable(dt)
  expect_equal(class(unlist(completeCases$variableDetails)), 'character')
  expect_equal(class(unlist(completeCases$completeCases)), 'integer')
  sampleSizes <- sampleSizeTable(dt)
  expect_equal(class(unlist(sampleSizes$panel)), 'character')
  expect_equal(class(unlist(sampleSizes$size)), 'integer')
})

test_that("box.dt() returns an appropriately sized data.table", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))


  map <- data.frame('id' = c('y', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable'), 'dataType' = c('NUMBER', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))

  dt <- box.dt(df, map, 'none', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label',  'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'mean'))

  dt <- box.dt(df, map, 'outliers', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers'))

  dt <- box.dt(df, map, 'outliers', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'outliers', 'mean'))

  dt <- box.dt(df, map, 'all', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData'))

  dt <- box.dt(df, map, 'all', TRUE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt),1)
  expect_equal(names(dt),c('label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence', 'rawData', 'mean'))

})
test_that("box.dt() accepts listVars for both x axis and facet vars", {  
  ## Case when we input multiple vars as one to x axis
  map <- data.frame('id' = c('y', 'x', 'z', 'group','cat2','cat3'), 'plotRef' = c('xAxisVariable', 'xAxisVariable', 'xAxisVariable', 'facetVariable1', 'facetVariable2', 'overlayVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING', 'STRING', 'STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  df[, z := x+y]
  df$cat2 <- sample(c('z1','z2','z3'), 500, replace=T) # Add another categorical var
  df$cat3 <- sample(c('a','b','c','d'), 500, replace=T) # Add another categorical var
  
  dt <- box.dt(df, map, 'none', FALSE, computeStats = T)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 4)
  expect_equal(names(dt),c('group', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$label)[[1]], c('x','y','z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')
  expect_equal(attr(dt, 'xAxisVariable')$variableId, 'meltedVariable')
  
  # testing json
  attrs <- attributes(dt)
  cct <- attr(dt, 'completeCasesTable')
  sst <- attr(dt, 'sampleSizeTable')
  st <- attr(dt, 'statsTable')
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  ## Case when we input multiple vars as one to facet 1
  map <- data.frame('id' = c('y', 'x', 'z', 'group'), 'plotRef' = c('facetVariable1', 'facetVariable1', 'facetVariable1', 'xAxisVariable'), 'dataType' = c('NUMBER', 'NUMBER', 'NUMBER','STRING'), 'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

  dt <- box.dt(df, map, 'none', FALSE)
  expect_is(dt, 'data.table')
  expect_equal(nrow(dt), 3)
  expect_equal(names(dt),c('meltedVariable', 'label', 'min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence'))
  expect_equal(unique(dt$meltedVariable), c('x','y','z'))
  expect_equal(attr(dt, 'yAxisVariable')$variableId, 'meltedValue')
  expect_equal(attr(dt, 'facetVariable1')$variableId, 'meltedVariable')
  
  # testing json
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  
  # lapply(lapply(dt$xVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId), list)
  namedAttrList <- getPDAttributes(dt)
  facet1 <- namedAttrList$facetVariable1$variableId
  names(dt)[names(dt) == facet1] <- 'facetVariableDetails'
  dt$facetVariableDetails <- lapply(lapply(dt$facetVariableDetails, makeVariableDetails, facet1, namedAttrList$facetVariable1$entityId), list)
  
  names(dt)[names(dt) == 'group'] <- 'overlayVariableDetails'
  ovd <- lapply(dt$overlayVariableDetails, makeVariableDetails, group, namedAttrList$overlayVariable$entityId)
  
})

test_that("box() returns appropriately formatted json", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy

  dt <- box.dt(df, map, 'none', FALSE)
  outJson <- getJSON(dt, FALSE)
  jsonList <- jsonlite::fromJSON(outJson)
  expect_equal(names(jsonList), c('boxplot','sampleSizeTable','statsTable','completeCasesTable'))
  expect_equal(names(jsonList$boxplot), c('data','config'))
  expect_equal(names(jsonList$boxplot$data), c('overlayVariableDetails','label','min','q1','median','q3','max','lowerfence','upperfence'))
  expect_equal(names(jsonList$boxplot$config), c('completeCases','plottedIncompleteCases','xVariableDetails','yVariableDetails'))
  expect_equal(names(jsonList$sampleSizeTable), c('overlayVariableDetails','xVariableDetails','size'))
  expect_equal(names(jsonList$completeCasesTable), c('variableDetails','completeCases'))
  expect_equal(names(jsonList$statsTable), c('panel','statistics','overlayVariableDetails'))
})

test_that("box.dt() returns correct information about missing data", {
  map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  df <- data.xy
  
  # Add 10 missing values to each column
  df$x[sample(1:100, 10, replace=F)] <- NA
  df$y[sample(1:100, 10, replace=F)] <- NA
  df$group[sample(1:100, 10, replace=F)] <- NA
  df$panel[sample(1:100, 10, replace=F)] <- NA
  dt <- box.dt(df, map, 'none', FALSE)
  completecasestable <- completeCasesTable(dt)
  # Each entry should equal NROW(df) - 10
  expect_equal(all(completecasestable$completeCases == nrow(df)-10), TRUE)
  # number of completeCases should be <= complete cases for each var
  expect_equal(all(attr(dt, 'completeCases')[1] <= completecasestable$completeCases), TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], 0)
  dt <- box.dt(df, map, points = 'none', mean = FALSE, computeStats = TRUE, evilMode = TRUE)
  expect_equal(attr(dt, 'plottedIncompleteCases')[1], sum(is.na(df$group) & !is.na(df$y) & !is.na(df$panel))) 
})

test_that("box.dt() returns an appropriately sized statistics table", {
  map <- data.frame('id' = c('xcat', 'y'), 'plotRef' = c('xAxisVariable', 'yAxisVariable'), 'dataType' = c('STRING', 'NUMBER'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS'), stringsAsFactors=FALSE)
  df <- as.data.frame(data.xy)
  df$xcat <- sample(c('x1','x2','x3'), 500, replace=T) # Add another categorical var
  df$zcat <- sample(c('z1','z2','z3'), 500, replace=T) # Add another categorical var
  
  # No overlay, no facets
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), 1)
  expect_equal(ncol(statsTable), 1)
  
  # No overlay, one facet
  map <- data.frame('id' = c('xcat', 'y', 'panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$panel))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay, no facets
  map <- data.frame('id' = c('xcat', 'y', 'group'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$xcat))
  expect_equal(ncol(statsTable), 2)
  
  # With overlay and facet
  map <- data.frame('id' = c('xcat', 'y', 'group', 'panel'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$xcat)*uniqueN(df$panel))
  expect_equal(ncol(statsTable), 3)
  
  # With overlay and two facets
  map <- data.frame('id' = c('xcat', 'y', 'group', 'panel', 'zcat'), 'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1', 'facetVariable2'), 'dataType' = c('STRING', 'NUMBER', 'STRING', 'STRING', 'STRING'), 'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL', 'CATEGORICAL', 'CATEGORICAL'), stringsAsFactors=FALSE)
  dt <- box.dt(df, map, 'none', FALSE, TRUE)
  statsTable <- attr(dt, 'statsTable')
  expect_equal(nrow(statsTable), uniqueN(df$xcat)*uniqueN(df$panel))
  expect_equal(ncol(statsTable), 3)
  
  
})

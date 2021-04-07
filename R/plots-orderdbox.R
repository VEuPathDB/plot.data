newOrderedBoxPD <- function(.dt = data.table::data.table(),
                          xAxisVars = list('variableId' = list(),
                                           'entityId' = list(),
                                           'dataType' = list()),
                          overlayVariable = list('variableId' = NULL,
                                                 'entityId' = NULL,
                                                 'dataType' = NULL),
                          facetVariable1 = list('variableId' = NULL,
                                                'entityId' = NULL,
                                                'dataType' = NULL),
                          facetVariable2 = list('variableId' = NULL,
                                                'entityId' = NULL,
                                                'dataType' = NULL),
                          points = character(),
                          mean = character(),
                          ...,
                          class = character()) {
  
  
  # All xAxisVar dataTypes must be numeric
  
  # All xAxisVar entityIDs must be the same
  
  # Note the initial order of the list determines the ordering of the boxes.
  
  
  longdt <- melt(.dt,
                 id.vars = c(overlayVariable$variableId, facetVariable1$variableId, facetVariable2$variableId),
                 measure.vars = xAxisVars$variableId)
  
  # Now "value" will be y and "variable" will be x.
  xAxisVariable <- list('variableId' = 'variable',
                        'entityId' = NULL,
                        'dataType' = 'STRING')
  yAxisVariable <- list('variableId' = 'value',
                        'entityId' = NULL,
                        'dataType'= 'NUMBER')

  
  
  .box <- newBoxPD(.dt = longdt,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points='none',
                    mean=FALSE)
  
  
  ## Order mybox appropriately
  attr <- attributes(.box)
  varOrder <- xAxisVars$variableId
  currentVarOrder <- .orderedBox[1, variable][[1]]
  
  # Do these match? If not, reorder .box
  if (!identical(varOrder, currentVarOrder)) {
    index <- match(orderedVars, currentVarOrder)
  
    # .box <- setBoxOrder(.box = .box, varOrder = varOrder)
    cols <- c('variable','min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence')
    for (j in cols) {
      print(j)
      set(.box, j = j, value = lapply(.box[[j]],function(vals) {vals[index]}))
    }
    
  }
  
  attr$varOrder <- varOrder
  attributes(.box) <- attr
  # setAttrFromList
  # Add order attribute
  
  return(.box)
  
}


orderedBox.dt <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  
  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  
  # Massage input
  plotRef = 'xAxisList'
  xAxisVars <- list('variableId' = unlist(str_split(map$id[map$plotRef == plotRef], ", ")),
                    'entityId' = unlist(str_split(map$entityId[map$plotRef == plotRef], ", ")),
                    'dataType' = unlist(str_split(map$dataType[map$plotRef == plotRef], ", ")))
  
  # Call combobox constructor
  .orderedBox <- newOrderedBoxPD(.dt = .dt,
                             xAxisVars = xAxisVars,
                             overlayVariable = overlayVariable,
                             facetVariable1 = facetVariable1,
                             facetVariable2 = facetVariable2,
                             points,
                             mean)
  
  # .orderedBox <- validateOrderedBoxPD(.orderedBox)
  
  return(.orderedBox)
  
  
}

orderedBox <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
points <- match.arg(points)
if (!mean %in% c(FALSE, TRUE)) { 
  stop('invalid input to argument `mean`.') 
}
.orderedBox <- orderedBox.dt(data, map, points, mean)

#### Are we going to expect the same boxplot plot component to render this? I think yes...
outFileName <- writeJSON(.orderedBox, 'boxplot')

return(outFileName))

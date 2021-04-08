#### QUESTION Should this be a type- file instead?
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
  
  
  # Note the initial order of the list determines the ordering of the boxes.
  
  # Reshape to a long data table
  .longdt <- data.table::melt(.dt,
                 id.vars = c(overlayVariable$variableId, facetVariable1$variableId, facetVariable2$variableId),
                 measure.vars = xAxisVars$variableId)
  
  # Now "value" will be y and "variable" will be x.
  #### UPDATE - entityId should be the shared entity o f xAxisVars
  xAxisVariable <- list('variableId' = 'variable',
                        'entityId' = NULL,
                        'dataType' = 'STRING')
  yAxisVariable <- list('variableId' = 'value',
                        'entityId' = NULL,
                        'dataType'= 'NUMBER')

  
  
  .box <- newBoxPD(.dt = .longdt,
                    xAxisVariable = xAxisVariable,
                    yAxisVariable = yAxisVariable,
                    overlayVariable = overlayVariable,
                    facetVariable1 = facetVariable1,
                    facetVariable2 = facetVariable2,
                    points=points,
                    mean=mean)
  
  
  ## Order mybox appropriately
  attr <- attributes(.box)
  varOrder <- xAxisVars$variableId
  currentVarOrder <- .box[1, variable][[1]]
  
  # Do these match? If not, reorder .box
  if (!identical(varOrder, currentVarOrder)) {
    index <- match(varOrder, currentVarOrder)
  
    # .box <- setBoxOrder(.box = .box, varOrder = varOrder)
    cols <- c('variable','min', 'q1', 'median', 'q3', 'max', 'lowerfence', 'upperfence')
    if (mean) {
      cols <- append(cols, 'mean')
    }
    #### UPDATE - the outliers will be wrong. Needs rework
    #### UPDATE - write function into something else more helpful
    .box[ , (cols) := lapply(.SD, function(vals, index) {vals[[1]][index]}, index), .SDcols = cols]
    
  }
  
  attr$varOrder <- varOrder
  attributes(.box) <- attr
  # setAttrFromList
  # Add order attribute
  
  return(.box)
  
}


validateOrderedBoxPD <- function(.orderedBox) {
  
  
  #### UPDATE - add validateBox
  
  #### UPDATE - add order 
  
  return(.orderedBox)
}



#### UPDATE - add docs
orderedBox.dt <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }
  
  overlayVariable = list('variableId' = NULL,
                         'entityId' = NULL,
                         'dataType' = NULL)
  facetVariable1 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  facetVariable2 = list('variableId' = NULL,
                        'entityId' = NULL,
                        'dataType' = NULL)
  
  
  if (!'data.table' %in% class(data)) {
    data <- data.table::as.data.table(data)
  }
  #### UPDATE - could be better at getting xAxisList to xAxisVars
  #### QUESTION - will there be other cases where we need a list of vars? Could write a function...
  if ('xAxisList' %in% map$plotRef) {
    xAxisVars <- list('variableId' = unlist(stringr::str_split(map$id[map$plotRef == 'xAxisList'], ", ")),
                      'entityId' = unlist(stringr::str_split(map$entityId[map$plotRef == 'xAxisList'], ", ")),
                      'dataType' = unlist(stringr::str_split(map$dataType[map$plotRef == 'xAxisList'], ", ")))
    if (any(xAxisVars$dataType != "NUMBER")) {
      stop("All variables in xAxisList must be of type NUMBER")
    }
    #### UPDATE add check for lengths of variableId, etc.
    #### UPDATE add check for all entityIds the same
  } else {
    stop("Must provide xAxisList for plot type box.")
  }
  if ('overlayVariable' %in% map$plotRef) {
    overlayVariable <- plotRefMapToList(map, 'overlayVariable')
  }
  if ('facetVariable1' %in% map$plotRef) {
    facetVariable1 <- plotRefMapToList(map, 'facetVariable1')
  }
  if ('facetVariable2' %in% map$plotRef) {
    facetVariable2 <- plotRefMapToList(map, 'facetVariable2')
  }
  
  .orderedBox <- newOrderedBoxPD(.dt = data,
                             xAxisVars = xAxisVars,
                             overlayVariable = overlayVariable,
                             facetVariable1 = facetVariable1,
                             facetVariable2 = facetVariable2,
                             points,
                             mean)
  
  .orderedBox <- validateOrderedBoxPD(.orderedBox)
  
  return(.orderedBox)

}




#### UPDATE - add docs
orderedBox <- function(data, map, points = c('outliers', 'all', 'none'), mean = c(FALSE, TRUE)) {
  points <- match.arg(points)
  if (!mean %in% c(FALSE, TRUE)) { 
    stop('invalid input to argument `mean`.') 
  }
  .orderedBox <- orderedBox.dt(data, map, points, mean)
  
  #### QUESTION Are we going to expect the same boxplot plot component to render this? 
  outFileName <- writeJSON(.orderedBox, 'orderedboxplot')
  
  return(outFileName)

}

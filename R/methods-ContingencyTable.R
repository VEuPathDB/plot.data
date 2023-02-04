setGeneric("orderByReferenceValues",
  function(object) standardGeneric("orderByReferenceValues"),
  signature = "object"
)

setMethod("orderByReferenceValues", signature("TwoByTwoTable"), function(object) {
  tbl <- object@data
  columnReferenceValue <- object@columnReferenceValue
  rowReferenceValue <- object@rowReferenceValue

  if (!is.na(columnReferenceValue)) {
    if (attributes(tbl)$dimnames[[2]][1] != columnReferenceValue) {
      attributes(tbl)$dimnames[[2]] <- rev(attributes(tbl)$dimnames[[2]])
      a <- tbl[1]; b <- tbl[2]; c <- tbl[3]; d <- tbl[4]
      tbl[1] <- c; tbl[2] <- d; tbl[3] <- a; tbl[4] <- b
    }
  }

  if (!is.na(rowReferenceValue)) {
    if (attributes(tbl)$dimnames[[1]][1] != rowReferenceValue) {
      attributes(tbl)$dimnames[[1]] <- rev(attributes(tbl)$dimnames[[1]])
      a <- tbl[1]; b <- tbl[2]; c <- tbl[3]; d <- tbl[4]
      tbl[1] <- b; tbl[2] <- a; tbl[3] <- d; tbl[4] <- c
    }
  }
  
  return(TwoByTwoTable('data' = tbl, 'columnReferenceValue' = columnReferenceValue, 'rowReferenceValue' = rowReferenceValue))
})

#' Chi-square Resultscheck for NA but ignore NaN
#' 
#' This function calculates Chi Square Results for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @importFrom stats chisq.test
#' @export
setGeneric("chiSqResults", 
  function(object) standardGeneric("chiSqResults"),
  signature = "object"
)

#' @export
setMethod("chiSqResults", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  chisq <- try(suppressWarnings(stats::chisq.test(tbl)))

  if (veupathUtils::is.error(chisq)) {
    return(veupathUtils::Statistic('name'='chiSq', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  stat <- veupathUtils::Statistic('name'='chiSq', 
                     'value'=signif(as.numeric(chisq$statistic), 2),
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=formatPValue(as.numeric(chisq$p.value)))

  return(stat)
})

#' fisher's Exact Test
#' 
#' This function calculates fisher's Exact Test for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @importFrom stats fisher.test
#' @export
setGeneric("fishersTest", 
  function(object) standardGeneric("fishersTest"),
  signature = "object"
)

#' @export
setMethod("fishersTest", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  fisher <- try(suppressWarnings(stats::fisher.test(tbl)))

  if (veupathUtils::is.error(fisher)) {
    return(veupathUtils::Statistic('name'='fisher', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  stat <- veupathUtils::Statistic('name'='fisher', 
                     'value'=signif(as.numeric(fisher$estimate), 2),
                     'confidenceInterval'=veupathUtils::Range('minimum'=signif(fisher$conf.int[1],2), 'maximum'=signif(fisher$conf.int[2],2)),
                     'confidenceLevel'=attr(fisher$conf.int, 'conf.level'),
                     'pvalue'=formatPValue(as.numeric(fisher$p.value)))

  return(stat)
})

#' Prevalence
#' 
#' This function calculates Prevalence for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("prevalence", 
  function(object) standardGeneric("prevalence"),
  signature = "object"
)

#' @export
setMethod("prevalence", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='prevalence', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- a+c
  denominator <- a+b+c+d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='prevalence', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_real_)

  return(stat)
})

#' Relative Risk
#' 
#' This function calculates relative risk for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("relativeRisk", 
  function(object) standardGeneric("relativeRisk"),
  signature = "object"
)

#' @export
setMethod("relativeRisk", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='relativeRisk', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- (a/(a+b))
  denominator <- (c/(c+d))
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='relativeRisk',
                    'value'=out$est, 
                    'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                    'confidenceLevel'=.95, 
                    'pvalue'=NA_real_)

  return(stat)  
})

#' Odds ratio
#' 
#' This function calculates Odds Ratio for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("oddsRatio", 
  function(object) standardGeneric("oddsRatio"),
  signature = "object"
)

#' @export
setMethod("oddsRatio", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='oddsRatio', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }

  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- (a*d)
  denominator <- (b*c)
  out <- zexact(numerator, denominator, .95)
 
  stat <- veupathUtils::Statistic('name'='oddsRatio',
                    'value'=out$est, 
                    'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                    'confidenceLevel'=.95, 
                    'pvalue'=NA_real_)

  return(stat) 
})

#' Sensitivity
#' 
#' This function calculates Sensitivity for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("sensitivity", 
  function(object) standardGeneric("sensitivity"),
  signature = "object"
)

#' @export
setMethod("sensitivity", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='sensitivity', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- a
  denominator <- a+c
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='sensitivity', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_real_)

  return(stat) 
})

#' Specificity
#' 
#' This function calculates Specificity for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("specificity", 
  function(object) standardGeneric("specificity"),
  signature = "object"
)

#' @export
setMethod("specificity", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='specificity', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- d
  denominator <- b+d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='specificity', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_real_)

  return(stat) 
})

#' Positivite Predictive Value
#' 
#' This function calculates positive predictive value for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("posPredictiveValue", 
  function(object) standardGeneric("posPredictiveValue"),
  signature = "object"
)

#' @export
setMethod("posPredictiveValue", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='posPredictiveValue', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- a
  denominator <- a+b
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='posPredictiveValue', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_real_)

  return(stat)
})

#' Negative Predictive Value
#' 
#' This function calculates negative predictive value for a contingency table.
#' @param object A TwoByTwoTable object
#' @return A veupathUtils::Statistic object
#' @export
setGeneric("negPredictiveValue", 
  function(object) standardGeneric("negPredictiveValue"),
  signature = "object"
)

#' @export
setMethod("negPredictiveValue", signature("TwoByTwoTable"), function(object) {
  object <- orderByReferenceValues(object)
  tbl <- object@data

  if (!length(tbl)) {
    return(veupathUtils::Statistic('name'='negPredictiveValue', 
                     'value'=NA_real_,
                     'confidenceInterval'=veupathUtils::Range(),
                     'confidenceLevel'=NA_real_,
                     'pvalue'=NA_real_))
  }
  
  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nRows > 1, tbl[2,1], 0)
  c <- ifelse(nCols > 1, tbl[1,2], 0)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], 0)

  numerator <- d
  denominator <- c+d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='negPredictiveValue', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_real_)

  return(stat)
})

#' All Available veupathUtils::Statistics for Contingency Table
#' 
#' This function calculates any and all statistics which are 
#' relevant for the class of contingency table that it receives.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A veupathUtils::StatisticList object
#' @export
setGeneric("allStats", 
  function(object) standardGeneric("allStats"),
  signature = "object"
)

#' @export
setMethod("allStats", signature("TwoByTwoTable"), function(object) {
   #TODO get list of methods from class and somehow automatically apply?
   return(veupathUtils::StatisticList(SimpleList(
    chiSqResults(object),
    fishersTest(object),
    oddsRatio(object),
    relativeRisk(object),
    prevalence(object),
    sensitivity(object),
    specificity(object),
    posPredictiveValue(object),
    negPredictiveValue(object)
   ))) 
})

#' Get TwoByTwoTable Named Quadrant Values
#' 
#' This function returns quadrant values for a 2x2 table named
#' a, b, c and d
#' @param object A TwoByTwoTable object
#' @return A named list
#' @export
setGeneric("getQuadrantValues",
  function(object, imputeZero = c(TRUE, FALSE)) standardGeneric("getQuadrantValues"),
  signature = "object"
)

#' @export
setMethod("getQuadrantValues", signature("TwoByTwoTable"), function(object, imputeZero = c(TRUE, FALSE)) {
  tbl <- object@data
  imputeZero <- veupathUtils::matchArg(imputeZero)
  emptyQuadrantValue <- NA_real_; if (imputeZero) emptyQuadrantValue <- 0

  nRows <- nrow(tbl)
  nCols <- ncol(tbl)

  a <- tbl[1,1]
  b <- ifelse(nCols > 1, tbl[1,2], emptyQuadrantValue)
  c <- ifelse(nRows > 1, tbl[2,1], emptyQuadrantValue)
  d <- ifelse(nRows > 1 && nCols > 1, tbl[2,2], emptyQuadrantValue)

  return(list('a'=a,'b'=b,'c'=c,'d'=d))
})

setGeneric("setQuadrantValues",
  function(object, quadrantValues) standardGeneric("setQuadrantValues"),
  signature = "object"
)

setMethod("setQuadrantValues", signature("TwoByTwoTable"), function(object, quadrantValues) {
  tbl <- object@data

  if (!is.na(quadrantValues$a)) tbl[1,1] <- quadrantValues$a
  if (!is.na(quadrantValues$b)) tbl[1,2] <- quadrantValues$b
  if (!is.na(quadrantValues$c)) tbl[2,1] <- quadrantValues$c
  if (!is.na(quadrantValues$d)) tbl[2,2] <- quadrantValues$d

  object@data <- tbl

  return(object)
})

#' Re-organize Contingency Table by Reference Values
#' 
#' This function guarantees a ContingencyTable or TwoByTwoTable object is organized
#' so that the accompanying statistics methods are calculated using the specified
#' reference values.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A ContingencyTable or TwoByTwoTable object
#' @export
setGeneric("orderByReferenceValues",
  function(object) standardGeneric("orderByReferenceValues"),
  signature = "object"
)

#' @export
setMethod("orderByReferenceValues", signature("TwoByTwoTable"), function(object) {
  tbl <- object@data
  columnReferenceValue <- object@columnReferenceValue
  rowReferenceValue <- object@rowReferenceValue

  if (!is.na(columnReferenceValue)) {
    if (attributes(tbl)$dimnames[[2]][1] != columnReferenceValue) {
      attr <- attributes(tbl)
      attr$dimnames[[2]] <- rev(attr$dimnames[[2]])
      quadrantValues <- getQuadrantValues(object, FALSE)
      a <- quadrantValues$b; c <- quadrantValues$d; b <- quadrantValues$a; d <- quadrantValues$c
      object <- setQuadrantValues(object, list('a'=a,'b'=b,'c'=c,'d'=d))
      tbl <- object@data
      attributes(tbl) <- attr
    }
  }

  if (!is.na(rowReferenceValue)) {
    if (attributes(tbl)$dimnames[[1]][1] != rowReferenceValue) {
      attr <- attributes(tbl)
      attr$dimnames[[1]] <- rev(attr$dimnames[[1]])
      quadrantValues <- getQuadrantValues(object, FALSE)
      a <- quadrantValues$c; c <- quadrantValues$a; b <- quadrantValues$d; d <- quadrantValues$b
      object <- setQuadrantValues(object, list('a'=a,'b'=b,'c'=c,'d'=d))
      tbl <- object@data
      attributes(tbl) <- attr
    }
  }
  
  return(TwoByTwoTable('data' = tbl, 'columnReferenceValue' = columnReferenceValue, 'rowReferenceValue' = rowReferenceValue))
})

#' Chi-square Results
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
                     'pvalue'=NA_character_))
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
                     'pvalue'=NA_character_))
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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  numerator <- quadrantValues$a + quadrantValues$c
  denominator <- quadrantValues$a + quadrantValues$b + quadrantValues$c + quadrantValues$d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='prevalence', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  RR <- (quadrantValues$a/(quadrantValues$a+quadrantValues$b)) / (quadrantValues$c/(quadrantValues$c+quadrantValues$d))
  alpha <- 0.05
  siglog <- sqrt((1/quadrantValues$a) + (1/quadrantValues$c) - (1/(quadrantValues$a+quadrantValues$b)) - (1/(quadrantValues$c+quadrantValues$d)))
  zalph <- qnorm(1 - alpha/2)
  logRR <- log(RR)
  logloRR <- logRR - zalph * siglog
  loghiRR <- logRR + zalph * siglog
  RRlo <- signif(exp(logloRR), 2)
  RRhi <- signif(exp(loghiRR), 2)

  stat <- veupathUtils::Statistic('name'='relativeRisk',
                    'value'=signif(RR, 2), 
                    'confidenceInterval'=veupathUtils::Range('minimum'=RRlo, 'maximum'=RRhi),
                    'confidenceLevel'=.95, 
                    'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }

  quadrantValues <- getQuadrantValues(object)

  OR <- (quadrantValues$a*quadrantValues$d)/(quadrantValues$b*quadrantValues$c)
  alpha <- 0.05
  siglog <- sqrt((1/quadrantValues$a) + (1/quadrantValues$b) + (1/quadrantValues$c) + (1/quadrantValues$d))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  logloOR <- logOR - zalph * siglog
  loghiOR <- logOR + zalph * siglog
  ORlo <- signif(exp(logloOR), 2)
  ORhi <- signif(exp(loghiOR), 2)
 
  stat <- veupathUtils::Statistic('name'='oddsRatio',
                    'value'=signif(OR, 2), 
                    'confidenceInterval'=veupathUtils::Range('minimum'=ORlo, 'maximum'=ORhi),
                    'confidenceLevel'=.95, 
                    'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  numerator <- quadrantValues$a
  denominator <- quadrantValues$a+quadrantValues$c
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='sensitivity', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  numerator <- quadrantValues$d
  denominator <- quadrantValues$b+quadrantValues$d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='specificity', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  numerator <- quadrantValues$a
  denominator <- quadrantValues$a+quadrantValues$b
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='posPredictiveValue', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_character_)

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
                     'pvalue'=NA_character_))
  }
  
  quadrantValues <- getQuadrantValues(object)

  numerator <- quadrantValues$d
  denominator <- quadrantValues$c+quadrantValues$d
  out <- zexact(numerator, denominator, .95)

  stat <- veupathUtils::Statistic('name'='negPredictiveValue', 
                     'value'=out$est,
                     'confidenceInterval'=veupathUtils::Range('minimum'=out$low, 'maximum'=out$upp),
                     'confidenceLevel'=.95,
                     'pvalue'=NA_character_)

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

#' @importFrom S4Vectors SimpleList
#' @export
setMethod("allStats", signature("TwoByTwoTable"), function(object) {
   #TODO get list of methods from class and somehow automatically apply?
   return(veupathUtils::StatisticList(S4Vectors::SimpleList(
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

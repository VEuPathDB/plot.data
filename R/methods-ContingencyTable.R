#' Chi-square Results
#' 
#' This function calculates Chi Square Results for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("chiSqResults", 
  function(object) standardGeneric("chiSqResults"),
  signature = "object"
)

#' @export
setMethod("chiSqResults", signature("TwoByTwoTable"), function(object) {
    
})

#' Fischer's Exact Test
#' 
#' This function calculates Fischer's Exact Test for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("fischersTest", 
  function(object) standardGeneric("fischersTest"),
  signature = "object"
)

#' @export
setMethod("fischersTest", signature("TwoByTwoTable"), function(object) {
    
})

#' Prevalence
#' 
#' This function calculates Prevalence for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("prevalence", 
  function(object) standardGeneric("prevalence"),
  signature = "object"
)

#' @export
setMethod("prevalence", signature("TwoByTwoTable"), function(object) {
    
})

#' Relative Risk
#' 
#' This function calculates relative risk for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("relativeRisk", 
  function(object) standardGeneric("relativeRisk"),
  signature = "object"
)

#' @export
setMethod("relativeRisk", signature("TwoByTwoTable"), function(object) {
    
})

#' Odds ratio
#' 
#' This function calculates Odds Ratio for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("oddsRatio", 
  function(object) standardGeneric("oddsRatio"),
  signature = "object"
)

#' @export
setMethod("oddsRatio", signature("TwoByTwoTable"), function(object) {
    
})

#' Sensitivity
#' 
#' This function calculates Sensitivity for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("sensitivity", 
  function(object) standardGeneric("sensitivity"),
  signature = "object"
)

#' @export
setMethod("sensitivity", signature("TwoByTwoTable"), function(object) {
    
})

#' Specificity
#' 
#' This function calculates Specificity for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("specificity", 
  function(object) standardGeneric("specificity"),
  signature = "object"
)

#' @export
setMethod("specificity", signature("TwoByTwoTable"), function(object) {
    
})

#' Positivite Predictive Value
#' 
#' This function calculates positive predictive value for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("posPredictiveValue", 
  function(object) standardGeneric("posPredictiveValue"),
  signature = "object"
)

#' @export
setMethod("posPredictiveValue", signature("TwoByTwoTable"), function(object) {
    
})

#' Negative Predictive Value
#' 
#' This function calculates negative predictive value for a contingency table.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A Statistic object
#' @export
setGeneric("negPredictiveValue", 
  function(object) standardGeneric("negPredictiveValue"),
  signature = "object"
)

#' @export
setMethod("negPredictiveValue", signature("TwoByTwoTable"), function(object) {
    
})

#' All Available Statistics for Contingency Table
#' 
#' This function calculates any and all statistics which are 
#' relevant for the class of contingency table that it receives.
#' @param object A ContingencyTable or TwoByTwoTable object
#' @return A StatisticList object
#' @export
setGeneric("allStats", 
  function(object) standardGeneric("allStats"),
  signature = "object"
)

#' @export
setMethod("allStats", signature("TwoByTwoTable"), function(object) {
   #TODO get list of methods from class and somehow automatically apply?
   return(StatisitcList(SimpleList(
    chiSqResults(object),
    fischersTest(object),
    oddsRatio(object),
    relativeRisk(object),
    prevalence(object),
    sensitivity(object),
    specificity(object),
    posPredictiveValue(object),
    negPredictiveValue(object)
   ))) 
})
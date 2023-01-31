check_statistic_with_confidence <- function(object) {
    errors <- character()
    value <- object@value
    pvalue <- object@pvalue
    ciMin <- object@confidenceInterval@minimum
    ciMax <- object@confidenceInterval@maximum
    ciLevel <- object@confidenceLevel

    if (length(value) != 1) {
        msg <- "The slot `value` must have a single value."
        errors <- c(errors, msg)
    } else

    if (!is.na(pvalue)) {
      if (length(pvalue) != 1) {
        msg <- "The slot `pvalue` must have a single value."
        errors <- c(errors, msg)
      } else if (pvalue < 0 || pvalue > 1) {
        msg <- "Provided p-value is invalid."
        errors <- c(errors, msg)
      }
    }  

    # false for NaN, while is.na is TRUE for NaN
    is.NA <- function(object) { ifelse(is.na(object), ifelse(is.nan(object), FALSE, TRUE), FALSE) }

    if (is.NA(ciMax) || is.NA(ciMin)) {
      if (!is.na(ciLevel)) {
        msg <- "A confidence level was provided without a confidence interval."
        errors <- c(errors, msg)
      }
    } else {
      #just to guarantee the below condition works
      if (is.nan(ciMin)) ciMin <- -Inf
      if (is.nan(ciMax)) ciMax <- Inf

      if (value < ciMin || value > ciMax) {
        msg <- "Provided value is not within the specified confidence interval."
        errors <- c(errors, msg)
      }
      if (ciLevel < 0 || ciLevel > 1) {
        msg <- "Provided confidence level is invalid."
        errors <- c(errors, msg)
      }
    }

    
 
    return(if (length(errors) == 0) TRUE else errors)
}

#' Statistic
#' 
#' A class to specify a named statistic, its value, confidence interval and p-value.
#' This is primarily to help maintain consistency in meeting the EDA API.
#' 
#' @slot name A string specifying what statistics was calculated
#' @slot value A number
#' @slot confidenceInterval An optional Range specifying minimum and maximum values for the confidence interval
#' @slot confidenceLevel An optional decimal number indicating the degree of confidence represented by the confidence interval
#' @slot pvalue A number
#' 
#' @name Statistic-class
#' @rdname Statistic-class
#' @export 
Statistic <- setClass("Statistic", representation(
    name = 'character',
    value = 'numeric',
    confidenceInterval = 'Range',
    confidenceLevel = 'numeric',
    pvalue = 'numeric'
), prototype = prototype(
    confidenceLevel = NA_real_
), validity = check_statistic_with_confidence)

#' @export
StatisticList <- setClass("StatisticList",
  contains = "SimpleList",
  prototype = prototype(elementType = "Statistic")
)
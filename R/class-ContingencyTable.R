# so S4 will recognize data.table class as inheriting from data.frame
setOldClass(c("data.table", "data.frame"))

check_contingency_table <- function(object) {
    errors <- character()
    df <- object@data
    colRef <- object@columnReferenceValue
    rowRef <- object@rowReferenceValue

    # check any specified references exist and have a single value
    if (!is.na(colRef)) {
        if (length(colRef) > 1) {
            msg <- "Column reference must have a single value."
            errors <- c(errors, msg)
        } else if (!colRef %in% names(df)) {
            msg <- "Specified column reference value does not exist in the provided data.frame."
            errors <- c(errors, msg)
        }
        if (is.na(rowRef)) {
            msg <- "Column reference value was provided without a corresponding row reference value."
            errors <- c(errors, msg)
        } else {
            if (length(rowRef) > 1) {
                msg <- "Row reference must have a single value."
                errors <- c(errors, msg)
            } else if (!rowRef %in% rownames(df)) {
                msg <- "Specified row reference value does not exist in the provided data.frame."
                errors <- c(errors, msg)
            }
        }
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' Contingency Table
#' 
#' A class to store a contingency table as a data.frame, calculate associated statistics
#' and specify which values should be used as the reference when calculating statistics.
#' 
#' @slot data a data.frame representing the contingency table to calculate statistics for
#' @slot columnReferenceValue A string representing a value present in the column names of the contingency table
#' @slot rowReferenceValue A string representing a value present in the row names of the contingency table
#' 
#' @name ContingencyTable-class
#' @rdname ContingencyTable-class
#' @export 
ContingencyTable <- setClass("ContingencyTable", representation(
    data = 'data.frame',
    columnReferenceValue = 'character',
    rowReferenceValue = 'character'
), prototype = prototype(
    columnReferenceValue = NA_character_,
    rowReferenceValue = NA_character_
), validity = check_contingency_table)

check_twobytwo_table <- function(object) {
    prev_check <- check_contingency_table(object)
    errors <- character()
    df <- object@data
    
    if (length(df) != 2 || nrow(df) != 2) {
      msg <- "Provided data.table is not the correct size. A table with two columns and two rows is required."
      errors <- c(errors, msg)
    }

    if (inherits(prev_check, 'character')) errors <- c(prev_check, errors)
    return(if (length(errors) == 0) TRUE else errors)
}

#' 2x2 Contingency Table
#' 
#' A class to store a contingency table as a data.frame, calculate associated statistics
#' and specify which values should be used as the reference when calculating statistics.
#' This variant must have two rows and two columns.
#' 
#' @slot data a data.frame representing the contingency table to calculate statistics for
#' @slot columnReferenceValue A string representing a value present in the column names of the contingency table
#' @slot rowReferenceValue A string representing a value present in the row names of the contingency table
#' 
#' @name TwoByTwoTable-class
#' @rdname TwoByTwoTable-class
#' @export 
TwoByTwoTable <- setClass("TwoByTwoTable", representation(
    data = 'data.frame',
    columnReferenceValue = 'character',
    rowReferenceValue = 'character'
), prototype = prototype(
    columnReferenceValue = NA_character_,
    rowReferenceValue = NA_character_
), validity = check_twobytwo_table)
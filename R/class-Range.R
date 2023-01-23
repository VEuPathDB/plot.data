check_range <- function(object) {
    errors <- character()
    min <- object@minimum
    max <- object@maximum

    if (class(min) != class(max)) {
       msg <- "Provided minimum and maximum values must be of the same base type (numeric, Date, Posix)."
       errors <- c(errors, msg) 
    }

    if (!inherits(min, c('numeric', 'Date', 'Posix'))) {
       msg <- "Provided minimum and maximum values must be numeric, Date, or Posix."
       errors <- c(errors, msg) 
    }
 
    return(if (length(errors) == 0) TRUE else errors)
}

#' Data Range
#' 
#' A class to a data range, represented as minimum and maximum values.
#' The range could be numeric or Date/ Posix
#' 
#' @slot minimum A number, Date or Posix value
#' @slot maximum A number, Date or Posix value
#' 
#' @name Range-class
#' @rdname Range-class
#' @export 
Range <- setClass("Range", representation(
    minimum = 'ANY',
    maximum = 'ANY'
), prototype = prototype(
    minimum = NA_real_,
    maximum = NA_real_
), validity = check_range)
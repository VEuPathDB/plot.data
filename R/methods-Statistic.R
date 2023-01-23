#' R object as JSON string
#' 
#' This function converts an R object to a JSON string.
#' see `methods(veupathUtils::toJSON)` for a list of support classes.
#' @param object object of a supported S4 class to convert to a JSON string representation
#' @param named logical indicating whether the result should be a complete named JSON object or just the value of the object
#' @return character vector of length 1 containing JSON string
#' @export
setGeneric("toJSON", 
  function(object, named = c(TRUE, FALSE)) standardGeneric("toJSON"),
  signature = "object"
)

#' @export
setMethod("toJSON", signature("Statistic"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    
    value_json <- jsonlite::toJSON(jsonlite::unbox(object@value))
    tmp <- paste0('"value":', value_json)

    ci_json <- plot.data::toJSON(object@confidenceInterval)
    tmp <- paste0(tmp, ',"confidenceInterval":', ci_json)

    conf_level_json <- jsonlite::toJSON(jsonlite::unbox(object@confidenceLevel))
    tmp <- paste0(tmp, ',"confidenceLevel":', conf_level_json)

    pvalue_json <- jsonlite::toJSON(jsonlite::unbox(object@pvalue))
    tmp <- paste0(tmp, ',"pvalue":', pvalue_json)

    if (named) {
      tmp <- paste0('{"', object@name, '":', tmp, "}")  
    }
    
    return(tmp)
})

# TODO consider moving this class to veupathUtils as well. avoid redundancy
setMethod("toJSON", signature("StatisticList"), function(object, named = c(TRUE, FALSE)) {
  named <- veupathUtils::matchArg(named)

  tmp <- as.list(S4SimpleList)
  tmp <- lapply(tmp, plot.data::toJSON, TRUE)
  tmp <- paste(tmp, collapse = ",")
  tmp <- paste0('[', tmp, ']')

  return(tmp)
})

#' Convert StatisicList to data.table
#' 
#' This function returns a data.table representation of a StatisticList.
#' The content of the slots are stored as JSON. Each entry in the list becomes a named column.
#' @param object A StatisticList object
#' @return data.table
#' @export
setGeneric("as.data.table", 
  function(object) standardGeneric("as.data.table"),
  signature = "object"
)

#' @export
setMethod("as.data.table", signature("StatisticList"), function(object) {
    colNames <- unlist(lapply(as.list(object), function(x) {x@name}))
    dt <- data.table::as.data.table(lapply(as.list(object), plot.data::toJSON))
    data.table::setnames(dt, colNames)

    return(dt)
})
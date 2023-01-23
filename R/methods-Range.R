# may make sense to move Range and methods to veupathUtils

#' @export
setMethod("toJSON", signature("Range"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 

    range_string <- paste0('(', object@minimum, ' - ', object@maximum, ')')
    range_json <- jsonlite::toJSON(jsonlite::unbox(range_string))

    if (named) {
      tmp <- paste0('{"range":', range_json, "}")  
    }
    
    return(tmp)
})
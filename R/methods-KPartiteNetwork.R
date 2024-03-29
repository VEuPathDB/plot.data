## Methods for Partitions
#' @include class-KPartiteNetwork.R
setGeneric("partitions", function(object) standardGeneric("partitions"))
setGeneric("partitions<-", function(object, value) standardGeneric("partitions<-"))

setMethod("partitions", "KPartiteNetwork", function(object) object@partitions)
setMethod("partitions<-", "KPartiteNetwork", function(object, value) {object@partitions <- value; validObject(object); object})

#' @include methods-Nodes.R
setGeneric("getAllNodeIds", function(object) standardGeneric("getAllNodeIds"))
setMethod("getAllNodeIds", "Partitions", function(object) unlist(lapply(as.list(object), getNodeIds)))

toJSONGeneric <- getGeneric("toJSON", package = "veupathUtils")

#' Convert Partitions object to JSON
#' 
#' Converts a Partitions object to JSON
#' @param object A Partitions object
#' @param named boolean that declares if names should be included
#' @export
setMethod(toJSONGeneric, "Partitions", function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named)
    tmp <- veupathUtils::S4SimpleListToJSON(object, TRUE)

    if (named) tmp <- paste0('{"partitions":', tmp, "}")

    return(tmp)
})

#' Convert KPartiteNetwork object to JSON
#' 
#' Converts a KPartiteNetwork object to JSON
#' @param object A KPartiteNetwork object
#' @param named boolean that declares if names should be included
#' @export 
setMethod(toJSONGeneric, "KPartiteNetwork", function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named)
    tmp <- character()

    nodes_json <- veupathUtils::toJSON(object@nodes, named = FALSE)
    links_json <- veupathUtils::toJSON(object@links, named = FALSE)
    partitions_json <- veupathUtils::toJSON(object@partitions, named = FALSE)
    
    tmp <- paste0('"nodes":', nodes_json, ',"links":', links_json, ',"partitions":', partitions_json)
    tmp <- paste0('"data":{', tmp, '}')
    tmp <- paste0('{', tmp, ',"config":{"variables":{', veupathUtils::toJSON(object@variableMapping, named = FALSE), '}}}')

    if (named) {
        name <- "kpartitenetwork"
        if (length(object@partitions) == 2) name <- "bipartitenetwork"
        tmp <- paste0('{"', name, '":', tmp, '}')
    }

    return(tmp)  
})


check_correlation_link <- function(object) {

  errors <- character()

  # check correlation coef (weight) and pvalue make some sense
  if (object@correlationCoef < -1 || object@correlationCoef > 1) {
    msg <- "Correlation coefficient must be between -1 and 1."
    errors <- c(errors, msg)
  }

  if (object@pValue < 0 || object@pValue > 1) {
    msg <- "P-value must be between 0 and 1."
    errors <- c(errors, msg)
  }

  # check that weight is the abs value of the correlation coefficient
  if (abs(object@correlationCoef) != object@weight) {
    msg <- "Weight must be the absolute value of the correlation coefficient."
    errors <- c(errors, msg)
  }

  return(if (length(errors) == 0) TRUE else errors) 
}

#' CorrelationLink
#' 
#' Represent one singular link in a correlation network. A link has a source,
#' a target, a correlation coefficient, and a p-value. Its weight is the
#' absolute value of the correlation coefficient. It is undirected.
#' It may have a color, timestamp, or label (coming soon!)
#' 
#' @name CorrelationLink-class
#' @rdname CorrelationLink-class
#' @include class-Node.R
#' @include constructors-Node.R
#' @include class-Link.R
#' @export
setClass("CorrelationLink",
  contains = "Link",
  slots = list(
    correlationCoef = "numeric",
    pValue = "numeric"
  ),
  prototype = prototype(
    source = NodeId(),
    target = NodeId(),
    color = NULL,
    isDirected = FALSE,
    correlationCoef = NA_real_,
    pValue = NA_real_,
    weight = NA_real_ # change default of 1 from parent Link class
  ),
  validity = check_correlation_link
)

#' CorrelationLink constructor
#' 
#' @param source The source node identifier
#' @param target The target node identifier
#' @param correlationCoef The correlation coefficient (weight) of the link
#' @param pValue The p-value of the link
#' @param color The color of the link
#' @export
#' @rdname CorrelationLink
setGeneric("CorrelationLink", 
function(
  source, 
  target, 
  correlationCoef = 1, 
  pValue = NULL, 
  color = NULL
) standardGeneric("CorrelationLink"), 
signature = c("source", "target"))

#' @rdname CorrelationLink
#' @aliases CorrelationLink,Node,Node-method
setMethod("CorrelationLink", c("Node", "Node"), function(source, target, correlationCoef, pValue, color = NULL) {
  new("CorrelationLink", 
    source = NodeId(id(source)), 
    target = NodeId(id(target)), 
    correlationCoef = correlationCoef, 
    pValue = pValue, 
    color = color,
    weight = abs(correlationCoef)
  )
})

#' @rdname CorrelationLink
#' @aliases CorrelationLink,character,character-method
setMethod("CorrelationLink", c("character", "character"), function(source, target, correlationCoef, pValue, color = NULL) {
  CorrelationLink(
    source = NodeId(source), 
    target = NodeId(target), 
    correlationCoef = correlationCoef, 
    pValue = pValue, 
    color = color
  )
})

#' @rdname CorrelationLink
#' @aliases CorrelationLink,numeric,numeric-method
setMethod("CorrelationLink", c("numeric", "numeric"), function(source, target, correlationCoef, pValue, color = NULL) {
  CorrelationLink(
    source = NodeId(source), 
    target = NodeId(target), 
    correlationCoef = correlationCoef, 
    pValue = pValue, 
    color = color
  )
})

#' @rdname CorrelationLink
#' @aliases CorrelationLink,NodeId,NodeId-method
setMethod("CorrelationLink", c("NodeId", "NodeId"), function(source, target, correlationCoef, pValue, color = NULL) {
  new("CorrelationLink", 
    source = source, 
    target = target, 
    correlationCoef = correlationCoef, 
    pValue = pValue, 
    color = color,
    weight = abs(correlationCoef)
  )
})

#' @rdname CorrelationLink
#' @aliases CorrelationLink,missing,missing-method
setMethod("CorrelationLink", c("missing", "missing"), function(source, target, correlationCoef, pValue, color = NULL) {
  new("CorrelationLink")
})

check_correlation_link_list <- function(object) {

  errors <- character()
  trueOrPrevErrors <- check_link_list(object)
  if (inherits(trueOrPrevErrors, "character")) {
    errors <- c(errors, trueOrPrevErrors)
  }

  # If one link has a correlationCoef, all must have a correlationCoef
  if (any(unlist(lapply(object, function(x) {!is.null(correlationCoef(x))})))) {
    if (!all(unlist(lapply(object, function(x) {!is.null(correlationCoef(x))})))) {
      errors <- c(errors, "If one link has a correlationCoef, all links must have a correlationCoef.")
    }
  }

  # If one link has a pValue, all must have a pValue
  if (any(unlist(lapply(object, function(x) {!is.null(pValue(x))})))) {
    if (!all(unlist(lapply(object, function(x) {!is.null(pValue(x))})))) {
      errors <- c(errors, "If one link has a pValue, all links must have a pValue.")
    }
  }

  return(if (length(errors) == 0) TRUE else errors) 

}

#' Correlation Link List
#' 
#' A class for representing links in a correlation network
#' 
#' @name CorrelationLinkList-class
#' @rdname CorrelationLinkList-class
#' @importFrom S4Vectors SimpleList
#' @export
setClass("CorrelationLinkList", 
  contains = "SimpleList",
  prototype = prototype(
    elementType = "CorrelationLink"
  ),
  validity = check_correlation_link_list
)

#' Generate a CorrelationLinkList
#' 
#' Generate a CorrelationLinkList from an edgeList
#' @param object Object containing data to be converted to a LinkList. Could be a SimpleList of Links or a data.frame
#' with columns source, target, correlationCoef, pValue.
#' @param linkColorScheme Either 'none' or 'posneg'. If 'posneg', the link color will be based on the sign of the correlation coefficient.
#' @param correlationCoefThreshold numeric value used to filter links based on correlationCoef. Default is NULL (i.e. no filtering).
#' Any links with an absolute correlationCoef below this threshold will be removed.
#' @param pValueThreshold numeric value used to filter links based on pValue. Default is NULL (i.e. no filtering).
#' Any links with an pValue above this threshold will be removed.
#' @return CorrelationLinkList
#' @export
#' @examples
#' CorrelationLinkList(data.frame(source='a',target='b',correlationCoef=0.5,pValue=0.01))
#' @rdname CorrelationLinkList
setGeneric("CorrelationLinkList", 
function(
  object, 
  linkColorScheme = c('none', 'posneg'),
  correlationCoefThreshold = NULL,
  pValueThreshold = NULL 
) standardGeneric("CorrelationLinkList"), signature = c("object"))

#' @rdname CorrelationLinkList
#' @aliases CorrelationLinkList,data.frame-method
setMethod("CorrelationLinkList", "data.frame", 
function(
  object = data.frame(
    source=character(),
    target=character(),
    correlationCoef=numeric(),
    pValue=numeric()
  ), 
  linkColorScheme = c('none', 'posneg'),
  correlationCoefThreshold = NULL,
  pValueThreshold = NULL
) {
  linkColorScheme <- veupathUtils::matchArg(linkColorScheme)
  
  if (!inherits(isValidEdgeList(object), "logical")) {
    stop(paste("Invalid edgeList:", isValidEdgeList(object), collapse = '\n'))
  }

  ## additional validation of correlationCoef and pValue in edgeList
  if (!all(c('correlationCoef', 'pValue') %in% colnames(object))) {
    stop("edgeList must contain columns 'correlationCoef' and 'pValue' for CorrelationLinkList/ CorrelationNetwork")
  }

  ## remove rows w NA correlationCoef, or not meeting thresholds
  object <- object[!is.na(object$correlationCoef), ]

  if (!is.null(correlationCoefThreshold)) {
    object <- object[abs(object$correlationCoef) >= correlationCoefThreshold, ]
  }
  if (!is.null(pValueThreshold)) {
    object <- object[object$pValue <= pValueThreshold, ]
  }

  if (nrow(object) == 0) {
    new("CorrelationLinkList")
  }

  makeLink <- function(rowInEdgeList, linkColorScheme) {
    source <- unname(rowInEdgeList['source'])
    target <- unname(rowInEdgeList['target'])
    correlationCoef <- as.numeric(unname(rowInEdgeList['correlationCoef']))
    pValue <- as.numeric(unname(rowInEdgeList['pValue']))

    if (linkColorScheme == 'posneg') {
      if (correlationCoef < 0) {
        color <- -1
      } else if (correlationCoef > 0) {
        color <- 1
      } else {
        color <- 0
      }
    } else {
      color <- NA_character_
    }
    
    if (is.na(color)) {
      link <- CorrelationLink(source, target, correlationCoef, pValue, NULL)
    } else {
      link <- CorrelationLink(source, target, correlationCoef, pValue, color)
    }

    return(link)
  }

  linkList <- apply(object, 1, makeLink, linkColorScheme)
  new("CorrelationLinkList", linkList)
})

#' @rdname CorrelationLinkList
#' @aliases CorrelationLinkList,missing-method
setMethod("CorrelationLinkList", "missing", function(object) {
  new("CorrelationLinkList")
})

#' @rdname CorrelationLinkList
#' @aliases CorrelationLinkList,SimpleList-method 
setMethod("CorrelationLinkList", "SimpleList", function(object) {
  new("CorrelationLinkList", object)
})

#' @rdname CorrelationLinkList
#' @aliases CorrelationLinkList,list-method
setMethod("CorrelationLinkList", "list", function(object) {
  new("CorrelationLinkList", object)
})
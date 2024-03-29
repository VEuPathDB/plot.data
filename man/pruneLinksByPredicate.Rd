% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-Network.R
\name{pruneLinksByPredicate}
\alias{pruneLinksByPredicate}
\title{Prune Links by Predicate}
\usage{
pruneLinksByPredicate(net, predicate, verbose = c(TRUE, FALSE), ...)
}
\arguments{
\item{net}{A Network object}

\item{predicate}{A function that takes a link and returns a boolean}

\item{verbose}{If TRUE, will print messages}

\item{...}{additional arguments passed to the predicate}
}
\description{
Removes links that satisfy a predicate
}

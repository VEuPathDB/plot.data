% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-stats.R
\name{fences}
\alias{fences}
\title{Fences}
\usage{
fences(x)
}
\arguments{
\item{x}{Numeric vector to calculate fences for}
}
\value{
Numeric vector of length 2: c(lowerfence, upperfence)
}
\description{
This function returns the lower and upper fences for a numeric
vector. The lower fence is calculated as the smallest value above `q1 - 1.5*IQR` 
and the upper as the largest value below `q3 + 1.5*IQR`.
}

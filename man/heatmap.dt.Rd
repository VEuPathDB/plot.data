% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-plotdata-heatmap.R
\name{heatmap.dt}
\alias{heatmap.dt}
\title{Heatmap as data.table}
\usage{
heatmap.dt(
  data,
  variables,
  value = c("series", "collection"),
  sampleSizes = c(TRUE, FALSE),
  completeCases = c(TRUE, FALSE),
  evilMode = c("noVariables", "allVariables", "strataVariables"),
  verbose = c(TRUE, FALSE)
)
}
\arguments{
\item{data}{data.frame to make plot-ready data for}

\item{variables}{veupathUtils::VariableMetadataList}

\item{value}{String indicating which of the three methods to use to calculate z-values ('collection', 'series')}

\item{sampleSizes}{boolean indicating if sample sizes should be computed}

\item{completeCases}{boolean indicating if complete cases should be computed}

\item{evilMode}{String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables')}

\item{verbose}{boolean indicating if timed logging is desired}
}
\value{
data.table plot-ready data
}
\description{
This function returns a data.table of 
plot-ready data with one row per group (per panel). Column 'table'
 contains a nested data.table of z-values for plotting. This 
table has a column for each x-axis entry and a row for each 
y-axis entry. Columns 'group' and 'panel' specify the group the 
series data belongs to. 
There are two ways to calculate z-values for the heatmap. \cr
1) 'collection' of numeric variables vs single categorical \cr
2) single numeric vs single categorical on a 'series' of dates
where yAxisVariable = categorical, xAxisVariable = date and zaxis = numeric
}
\section{Evil Mode}{

An `evilMode` exists. It will do the following: \cr
- when `strataVariables` it will return 'no data' as a regular value for strata vars but will discard such cases for the axes vars. \cr
- when `allVariables` it will return 'no data' as a regular value for all variables. \cr
- when `noVariables` it will do the sensible thing and return complete cases only. \cr
- not return statsTables \cr
- allow smoothed means and agg values etc over axes values where we have no data for the strata vars \cr
- return a total count of plotted incomplete cases \cr
- represent missingness poorly, conflate the stories of completeness and missingness, mislead you and steal your soul \cr
}


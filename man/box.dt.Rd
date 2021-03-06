% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-plotdata-box.R
\name{box.dt}
\alias{box.dt}
\title{Box Plot as data.table}
\usage{
box.dt(
  data,
  map,
  points = c("outliers", "all", "none"),
  mean = c(FALSE, TRUE),
  computeStats = c(TRUE, FALSE)
)
}
\arguments{
\item{data}{data.frame to make plot-ready data for}

\item{map}{data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'}

\item{points}{character vector indicating which points to return 'outliers' or 'all'}

\item{mean}{boolean indicating whether to return mean value per group (per panel)}

\item{computeStats}{boolean indicating whether to compute nonparametric statistical tests (across x values or group values per panel)}
}
\value{
data.table plot-ready data
}
\description{
This function returns a data.table of 
plot-ready data with one row per group (per panel). Columns 
'x', 'min', 'q1', 'median', 'q3' and 'max' represent the 
pre-computed values per group. Columns 'group' and 'panel' specify
the group the data belong to. 
Optionally, can return columns 'outliers' and 'mean' as well.
}

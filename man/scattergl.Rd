% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-plotdata-scatter.R
\name{scattergl}
\alias{scattergl}
\title{Scatter Plot data file}
\usage{
scattergl(
  data,
  map,
  value = c("smoothedMean", "smoothedMeanWithRaw", "bestFitLineWithRaw", "density",
    "raw")
)
}
\arguments{
\item{data}{data.frame to make plot-ready data for}

\item{map}{data.frame with at least two columns (id, plotRef) indicating a variable sourceId and its position in the plot. Recognized plotRef values are 'xAxisVariable', 'yAxisVariable', 'overlayVariable', 'facetVariable1' and 'facetVariable2'}

\item{value}{character indicating whether to calculate 'smoothedMean', 'bestFitLineWithRaw' or 'density' estimates (no raw data returned), alternatively 'smoothedMeanWithRaw' to include raw data with smoothed mean. Note only 'raw' is compatible with a continuous overlay variable.}
}
\value{
character name of json file containing plot-ready data
}
\description{
This function returns the name of a json file containing 
plot-ready data with one row per group (per panel). Columns 
'seriesX' and 'seriesY' contain the raw data for the 
scatter plot. Column 'group' and 'panel' specify the group the 
series data belongs to. Optionally, columns 'smoothedMeanX', 
'smoothedMeanY' and 'smoothedMeanSE' specify the x, y and 
standard error respectively of the smoothed conditional mean 
for the group. Columns 'densityX' and 'densityY' contain the 
calculated kernel density estimates. Column 'seriesGradientColorscale'
contains values to be used with a gradient colorscale when plotting.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-plotdata-beeswarm.R
\name{beeswarm.dt}
\alias{beeswarm.dt}
\title{Beeswarm Plot as data.table}
\usage{
beeswarm.dt(
  data,
  variables,
  jitter = NULL,
  median = c(FALSE, TRUE),
  overlayValues = NULL,
  sampleSizes = c(TRUE, FALSE),
  completeCases = c(TRUE, FALSE),
  evilMode = c("noVariables", "allVariables", "strataVariables"),
  verbose = c(TRUE, FALSE)
)
}
\arguments{
\item{data}{data.frame to make plot-ready data for}

\item{variables}{veupathUtils VariableMetadataList}

\item{jitter}{numeric indicating the maximum width by which to randomly offset points.}

\item{median}{boolean indicating whether to return median value per group (per panel)}

\item{overlayValues}{veupathUtils::BinList providing overlay values of interest}

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
plot-ready data with one row per group (per panel). Columns 
'label' and 'jitteredValues' represent the x axis tick label and a random offset (one per y value), repsectively.
The 'rawData' column lists the y values to be plotted above each x axis tick. Columns 'group' and 'panel' specify
the group the data belong to. 
Optionally, can return median values per group
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

\examples{
# Construct example data
df <- data.table('entity.xvar' = sample(c('a','b','c'), 100, replace=T),
                 'entity.yvar' = rnorm(100),
                 'entity.overlay' = sample(c('red','green','blue'), 100, replace=T))

# Create VariableMetadataList that specifies variable role in the plot and supplies variable metadata
variables <- veupathUtils::VariableMetadataList(
  veupathUtils::VariableMetadata(
    variableClass = veupathUtils::VariableClass(value = 'native'),
    variableSpec = veupathUtils::VariableSpec(variableId = 'xvar', entityId = 'entity'),
    plotReference = veupathUtils::PlotReference(value = 'xAxis'),
    dataType = veupathUtils::DataType(value = 'STRING'),
    dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
  ),
  veupathUtils::VariableMetadata(
    variableClass = veupathUtils::VariableClass(value = 'native'),
    variableSpec = veupathUtils::VariableSpec(variableId = 'overlay', entityId = 'entity'),
    plotReference = veupathUtils::PlotReference(value = 'overlay'),
    dataType = veupathUtils::DataType(value = 'STRING'),
    dataShape = veupathUtils::DataShape(value = 'CATEGORICAL')
  ),
  veupathUtils::VariableMetadata(
    variableClass = veupathUtils::VariableClass(value = 'native'),
    variableSpec = veupathUtils::VariableSpec(variableId = 'yvar', entityId = 'entity'),
    plotReference = veupathUtils::PlotReference(value = 'yAxis'),
    dataType = veupathUtils::DataType(value = 'NUMBER'),
    dataShape = veupathUtils::DataShape(value = 'CONTINUOUS')
  )
)

# Returns a data table with plot-ready data
dt <- beeswarm.dt(df, map, jitter=0.3)
}

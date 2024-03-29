% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-plotdata-bar.R
\name{bar}
\alias{bar}
\title{Bar Plot data file}
\usage{
bar(
  data,
  variables = variables,
  value = c("count", "identity", "proportion"),
  barmode = c("group", "stack"),
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

\item{value}{String indicating how to calculate y-values ('identity', 'count', 'proportion')}

\item{barmode}{String indicating if bars should be grouped or stacked ('group', 'stack')}

\item{overlayValues}{veupathUtils::BinList providing overlay values of interest}

\item{sampleSizes}{boolean indicating if sample sizes should be computed}

\item{completeCases}{boolean indicating if complete cases should be computed}

\item{evilMode}{String indicating how evil this plot is ('strataVariables', 'allVariables', 'noVariables')}

\item{verbose}{boolean indicating if timed logging is desired}
}
\value{
character name of json file containing plot-ready data
}
\description{
This function returns the name of a json file containing 
plot-ready data with one row per group (per panel). Columns 
'label' and 'value' contain the raw data for plotting. Column 
'group' and 'panel' specify the group the series data belongs to.
There are three options to calculate y-values for plotting. \cr
1) raw 'identity' of values from data.table input \cr
2) 'count' occurrences of values from data.table input \cr 
3) 'proportion' of occurrences of values from data.table input \cr
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
  )
)

# Returns the name of a json file
bar(df,map,value='count')
}

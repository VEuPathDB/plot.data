# plot.data

plot.data is an R package for creating client-ready data for various plots and visualizations. Data can be returned as either a data.table or a json file. The json file also includes some additional information helpful for rendering various plot widgets (ex: recommended range and step for a bin width slider to accompany a histogram).

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install plot.data. From the R command prompt:

```R
remotes::install_github('VEuPathDB/plot.data')

# or to install a specific version
remotes::install_github('VEuPathDB/plot.data', 'v1.2.3')
```

## Usage
### Example 1: Histogram
```R
# Data object is a data.table of raw values to bin and count
df <- data.table('xvar' = rnorm(100))

#map is a data.table mapping column names from data object to 'xAxisVariable', 'overlayVariable', etc
map <- data.frame(id = c('xvar'),
                  'plotRef' = c('xAxisVariable'),
                  'dataType' = c('NUMBER'),
                  'dataShape' = c('CONTINUOUS'), stringsAsFactors=FALSE)

# Returns the name of a json file where histogram-ready plotting data can be found
histogram(data, 
          map, 
          value='count', 
          binWidth=NULL, 
          binReportValue='binWidth', 
          viewport=NULL)
```

### Example 2: Scatter with overlay
```R
# Example dataset
df <- data.table('xvar' = rnorm(100),
                 'yvar' = rnorm(100),
                 'overlay' = sample(c('red','green','blue'), 100, replace=T))

# Create map that specifies variable role in the plot, and supplies variable metadata
map <- data.frame(id = c('xvar', 'yvar', 'overlay'),
                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'overlayVariable'),
                  'dataType' = c('NUMBER', 'NUMBER', 'STRING'),
                  'dataShape' = c('CONTINUOUS', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

# Returns the name of a json file where scatterplot-ready plotting data can be found.
scattergl(df,
          map,
          value='bestFitLineWithRaw')
```

### Example 3: Box with one facet variable
```R
# Example dataset
df <- data.table('xvar' = sample(letters[1:5], 100, replace=T),
                 'yvar' = rnorm(100),
                 'overlay' = sample(c('facet1','facet2','facet3'), 100, replace=T))

# Create map
map <- data.frame(id = c('xvar', 'yvar', 'overlay'),
                  'plotRef' = c('xAxisVariable', 'yAxisVariable', 'facetVariable1'),
                  'dataType' = c('STRING', 'NUMBER', 'STRING'),
                  'dataShape' = c('CATEGORICAL', 'CONTINUOUS', 'CATEGORICAL'), stringsAsFactors=FALSE)

# Returns the name of a json file where boxplot-ready plotting data can be found.
box(df,
    map,
    points='outliers',
    mean=F,
    computeStats=F)
```
## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Development
Before we begin, a few definitions:
- Variable: A collection of related values, which may represent either a category or a measurement. For example, a "Days of the Week" variable has values "Monday", "Tuesday", ... "Sunday". A variable can be categorical, date or numeric, and must have at least one value.
- Axis variable: A variable mapped to the independent (x) or dependent (y, z) axes.
- Strata variable: A variable used to partition the axis variables into groups. Strata variables include overlay (often color), and facets.
- Group: A partition of axes variable values labelled by combinations of strata variable values. For example, in a boxplot with an overlay variable with four values, we would see four groups - one for all the data associated with all boxes of the same color.
- Panel: A set of axes and groups necessary and sufficient to stand as an independent plot, but contains a partition of the data. Panels are defined by the interaction of values of the two facet variables. Defining only one facet will result in one panel per facet variable value.
- Variable constraint: A plot type \cap plot element specific restriction on the data type, shape, and/or number of unique values a variable contains. Implemented to prevent nonsensical plots, such as trying to create a boxplot with a categorical variable on the y-axis. For a more complete description, see (eventual link to site).
- Plot: A collection of required axes, optional parameters, variable constraints, and geometric shapes bound to data that together convey information about a dataset.  
- Visualization: A collection of one or more plot instances that serve to answer a question.
### Adding a new plot
If the goal is to add a new plot, we first ask if the addition should be an entirely new plot class, or an add-on to an existing plot class. We attempt to follow the following rule when deciding how and where to add new functionality:
> Rule: Plot classes correspond to abstract plot types.

Let's take the beeswarm plot as an illustrative example. Is a beeswarm a plot type distinct enough from both box and scatter to deserve its own class? The beeswarm is similar to box in that it is meant to show a distribution of a continuous variable split across a categorical variable. However, the beeswarm in itself does not require summary points such as median, quartiles, etc. Since a beeswarm maps samples to points, perhaps it should instead be an option in the scatter class? While true, note that the variable constraints for a beeswarm and a scatterplot differ: a beeswarm takes categorical variables on the independent axis while a scatterplot does not. Therefore, let's give the beeswarm its own class.

***plot.data class files***
Each `plot.data` class has a similar set up within their "class-plotdata-{plot name}.R" file:
- A function that creates a new instance of the class. These constructors are named "new{plot name}PD" (ex. `newBeeswarmPD`). Each constructor begins by creating a `plotdata` object (`newPlotdata`).
- A function that takes data and returns plot-ready data along with any statistics or other additional information requested. These functions are named "{plot name}.dt" (ex. `beeswarm.dt`). This function calls the class constructor.
- A function that takes data and returns a json file containing the above plot-ready data. We name these functions "{plot name}" (ex. `beeswarm`).
- Validation functions. For each class, we include at least one validation function that ensures the created plot-ready data adheres to the appropriate variable constraints, for example. We name these functions "validate{plot name}PD" (ex. `validateBeeswarmPD`).

***Testing***
Each plotdata class should have a corresponding test context, i.e file called "test-{plot name}.R" in the tests/testthat directory. Tests written in this file should be basic unit tests, for example checking that the created object is of the appropriate class and size. See `test-beeswarm.R` for an example.

The tests should follow the below general organization:
1. Check the returned object is of the appropriate size and shape.
2. Test that types are as expected.
3. Ensure a valid data.table is returned with expected dimensions, even when inputs are not idea (ex. factors, numeric categorical variables).
4. Validate the `getJSON` output structure.
5. Test that missing data is handled appropriately.
6. Vizualization-specific tests such as statistics.


***Helpers***
Helper functions are organized into those that compute values per group (`group.R`), per panel (`panel.R`), handle binning (`bin.R`), or various other categories (see `utils` and `utils-*.R`). Using the beeswarm as an example, we can add `groupMedian` to `group.R`, which computes the median of the dataset per group (overlay, panel).

***Exporting functions***
Now that we've created a new plot, we'd like to use it! Add relevant functions to `NAMESPACE` with `devtools::document()`, so the new functions will get properly exported and can be used when someone loads `plot.data`.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/VEuPathDB/plot.data/workflows/R-CMD-check/badge.svg)](https://github.com/VEuPathDB/plot.data/actions)
[![Codecov test coverage](https://codecov.io/gh/VEuPathDB/plot.data/branch/main/graph/badge.svg)](https://codecov.io/gh/VEuPathDB/plot.data?branch=main)
<!-- badges: end -->

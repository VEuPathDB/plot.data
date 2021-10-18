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

```R
#data object is a data.table of raw values to bin and count
#map is a data.table mapping column names from data object to 'xAxisVariable', 'overlayVariable', etc
#returns the name of a json file where client-ready plotting data can be found
histogram(data, 
          map, 
          value='count', 
          binWidth=NULL, 
          binReportValue='binWidth', 
          viewport=NULL)
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Development
Before we begin, a few definitions:
- Variable: Data belonging to one category or name. Variables contain *values*. (maybe: 'A collection of related values, which may represent either a category or a measurement.' -Danielle) For example, a "Days of the Week" variable has values "Monday", "Tuesday", ... "Sunday". A variable can be categorical, date or numeric, and must have at least one value.
- Axis: 
- Group: 
- Panel: An independent collection of axes.
- Strata variable: A variable that 
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
- Validation functions. For each class, we should include at least one validation function that ensures the created plot-ready data adheres to the appropriate variable constraints, for example. We name these functions "validate{plot name}PD" (ex. `validateBeeswarmPD`).

***Testing***
Each plotdata class should have a corresponding test context, i.e file called "test-{plot name}.R" in the tests/testthat directory. Tests written in this file should be basic unit tests, for example checking that the created object is of the appropriate class and size. See `test-beeswarm.R` for an example.

***Helpers***
Helper functions are organized into those that compute values per group (`group.R`), per panel (`panel.R`), handle binning (`bin.R`), or various other categories (see `utils` and `utils-*.R`). Using the beeswarm as an example, we can add `groupMedian` to `group.R`, which computes the median of the dataset per group (overlay, panel).

***Exporting functions***
Now that we've created a new plot, we'd like to use it! Add relevant functions to `NAMESPACE` so they get properly exported and can be used when someone loads `plot.data`. (This should be done with devtools::document(), rather than directly updating `NAMESPACE` -Danielle)

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/VEuPathDB/plot.data/workflows/R-CMD-check/badge.svg)](https://github.com/VEuPathDB/plot.data/actions)
[![Codecov test coverage](https://codecov.io/gh/VEuPathDB/plot.data/branch/main/graph/badge.svg)](https://codecov.io/gh/VEuPathDB/plot.data?branch=main)
<!-- badges: end -->

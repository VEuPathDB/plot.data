# plot.data

plot.data is an R package for creating client-ready data for various plots and visualizations. Data can be returned as either a data.table or a json file. The json file also includes some additional information helpful for rendering various plot widgets (ex: recommended range and step for a bin width slider to accompany a histogram).

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install plot.data. From the R command prompt:

```R
remotes::install_github('VEuPathDB/plot.data')
```

## Usage

```R
#data object is a data.table of raw values to bin and count
#map is a data.table mapping column names from data object to 'xAxisVariable', 'overlayVariable', etc
histogram(data, map, value='count', binWidth=NULL, binReportValue='binWidth', viewport=NULL) # returns the name of a json file where client-ready plotting data can be found
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Development
Before we begin, a few definitions:
- Variable: Data belonging to one category or name. Variables contain *values*. For example, a "Days of the Week" variable has values "Monday", "Tuesday", ... "Sunday". A variable can be categorical or numeric, and must have at least one value.
- Axis: 
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



## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

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

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/VEuPathDB/plot.data/workflows/R-CMD-check/badge.svg)](https://github.com/VEuPathDB/plot.data/actions)
[![Codecov test coverage](https://codecov.io/gh/VEuPathDB/plot.data/branch/main/graph/badge.svg)](https://codecov.io/gh/VEuPathDB/plot.data?branch=main)
<!-- badges: end -->

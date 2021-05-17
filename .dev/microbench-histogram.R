## Microbenchmarking
library(crayon)
source("./.dev/helpers-microbenchmark.R")

## Histogram
context <- "histogram"

# Boolean to decide if we overwrite old results. Overwrite before
# merging new feature. Will get overwritten by allOverwrite if running 
# from microbenchmark-all.R
overwrite <- T
if (exists("allOverwrite")) {
  overwrite <- allOverwrite
}

# Prepare dt
results_dt <- data.table()

# Load in allResults dt
allResults <- readRDS(file = "./.dev/benchmarks.rds")

# Currently taken from testing scripts
name <- "basic hist"

# Prep
map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(bigData)
viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
binReportValue <- 'binWidth'

# Benchmark
results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport),
  unit = 'ms'
  ) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)

results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



# Test 2
name <- "date hist"

# Prep
map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(data.dates)
viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
binReportValue <- 'binWidth'

# Benchmark
results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))




## Updating logged results
# If overwrite == T, replace saved times
if (overwrite) {
  
  cat(magenta("Overwriting previously saved benchmark times... \n \n"))
  
  # Remove all data from current context
  allResults <- allResults[benchmarkContext != context]
  
  # Add new results and save
  allResults <- rbind(allResults,results_dt)
  saveRDS(allResults, "./.dev/benchmarks.rds")
}


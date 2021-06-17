## Microbenchmarking
library(crayon)
source("./.dev/helpers-microbenchmark.R")

## Context (should match testthat context)
context <- "mosaic"

# Boolean to decide if we overwrite old results. Overwrite before
# merging new feature. Will get overwritten by allOverwrite if running 
# from microbenchmark-all.R
overwrite <- F
if (exists("allOverwrite")) {
  overwrite <- allOverwrite
}

# Prepare dt
results_dt <- data.table()

# Load in allResults dt
allResults <- readRDS(file = "./.dev/benchmarks.rds")

## Test 1
name <- "facet1"

# Prep
map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('yAxisVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'STRING', 'STRING'), stringsAsFactors=FALSE)

# Benchmark
results <- microbenchmark::microbenchmark(
  mosaic.dt(data.binary, map),
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


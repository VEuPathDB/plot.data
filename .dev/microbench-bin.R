## Microbenchmarking
library(crayon)
source("./.dev/helpers-microbenchmark.R")

## Context (should match testthat context)
context <- "bin"

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
name <- "y"

# Prep
viewport <- list('xMin'=min(data.xy$y), 'xMax'=max(data.xy$y))

# Benchmark
results <- microbenchmark::microbenchmark(
  binProportion(bigData.xy,'y', binWidth=.1, viewport=viewport),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)

results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



## Test 2
name <- "y group panel"

# Prep

# Benchmark
results <- microbenchmark::microbenchmark(
  binProportion(data.xy, 'y', 'group', 'panel', binWidth=.1, viewport=viewport),
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


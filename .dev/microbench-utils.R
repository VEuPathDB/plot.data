## Microbenchmarking
library(crayon)
source("./.dev/helpers-microbenchmark.R")

## Context (should match testthat context)
context <- "utils"

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
name <- "smoothedMean"

# Prep
dt <- data.table::data.table(x = c(1, 5, 2, 3, 4), y = 1:5)

# Benchmark
results <- microbenchmark::microbenchmark(
  smoothedMean(dt, 'loess'),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)

results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



## Test 2
name <- "epitabToDT"

# Prep
m <- epitools::epitab(as.factor(rnorm(10)), as.factor(rep(c(1,2),5)))$tab

# Benchmark
results <- microbenchmark::microbenchmark(
  epitabToDT(m, 'oddsratio'),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))




## Test 3
name <- "relativeRisk"

# Prep
data <- data.table('x' = as.factor(rnorm(10)), 'y' = as.factor(rep(c(1,2),5)))

# Benchmark
results <- microbenchmark::microbenchmark(
  relativeRisk(data),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



## Test 4
name <- "oddsRatio"

# Prep
data <- data.table('x' = as.factor(rnorm(10)), 'y' = as.factor(rep(c(1,2),5)))

# Benchmark
results <- microbenchmark::microbenchmark(
  oddsRatio(data),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



## Test 5
name <- "getAggStr"

# Prep

# Benchmark
results <- microbenchmark::microbenchmark(
  getAggStr(c('x','y'), c('group','panel')),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))


## Test 6
name <- "makePanels"

# Prep

# Benchmark
results <- microbenchmark::microbenchmark(
  panels <- makePanels(data, 'group', 'panel'),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult, context, name)


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))


## Test 7
name <- "contingencyDT"

# Prep

# Benchmark
results <- microbenchmark::microbenchmark(
  panels <- contingencyDT(data.binned),
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


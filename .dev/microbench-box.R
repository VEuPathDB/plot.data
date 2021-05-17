## Microbenchmarking
library(crayon)
source("./.dev/helpers-microbenchmark.R")

## Scatter
context <- "box"

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

# Currently taken from testing scripts
name <- "overlay outliers"

# Prep
map <- data.frame('id' = c('group', 'y', 'panel'), 'plotRef' = c('overlayVariable', 'yAxisVariable', 'xAxisVariable'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
df <- bigData.xy

# Benchmark
results <- microbenchmark::microbenchmark(
  box.dt(df, map, 'outliers', FALSE),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult)

results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



# Test 2
name <- "overlay all"

# Prep

# Benchmark
results <- microbenchmark::microbenchmark(
  box.dt(df, map, 'all', FALSE),
  unit = 'ms'
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
compareToPrevious(results, previousResult)


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


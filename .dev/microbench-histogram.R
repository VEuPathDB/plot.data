## Microbenchmarking
library(crayon)

## Histogram
context <- "histogram"

# Boolean to decide if we overwrite old results. Overwrite before
# merging new feature.
overwrite <- T

# Prepare dt
results_dt <- data.table()

# Load in allResults dt
allResults <- readRDS(file = "./dev/benchmarks.rds")

# if defined allOverwrite, change overwrite to that

# Currently taken from testing scripts
name <- "basic hist"

map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(bigData)
viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
binReportValue <- 'binWidth'

results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  ) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
if (NROW(previousResult == 1)) {
  if (results$median < previousResult$median) {
    cat(paste0(context,", ",name, ": ") %+% cyan(results$median - previousResult$median) %+% " milliseconds \n")
  } else {
    cat(paste0(context,", ",name, ": +") %+% red(results$median - previousResult$median) %+% " milliseconds \n")
  }
} else if(NROW(previousResult == 0)) {
  cat("No previous results for this test. \n") 
} else {
  cat("Benchmark test name and context not unique!")
}


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))



# Dates
name <- "date hist"

map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(data.dates)
viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
binReportValue <- 'binWidth'
results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
) %>% summary()

# Print diff from saved result
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
if (NROW(previousResult == 1)) {
  if (results$median < previousResult$median) {
    cat(paste0(context,", ",name, ": ") %+% cyan(results$median - previousResult$median) %+% " milliseconds \n")
  } else {
    cat(paste0(context,", ",name, ": +") %+% red(results$median - previousResult$median) %+% " milliseconds \n")
  }
} else if(NROW(previousResult == 0)) {
  cat("No previous results for this test. \n") 
} else {
  cat("Benchmark test name and context not unique!")
}
results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))


# If overwrite == T, replace saved times
if (overwrite) {
  
  # Remove all data from current context
  allResults <- allResults[benchmarkContext != context]
  
  # Add new results and save
  allResults <- rbind(allResults,results_dt)
  saveRDS(allResults, "./dev/benchmarks.rds")
}


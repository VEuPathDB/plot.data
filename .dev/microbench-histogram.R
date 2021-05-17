## Microbenchmarking
# Consider making data structure that holds the last run. Then you can always
# compare to the old run. We have to manually overwrite the data holding 
# benchmark vals


# Histogram
results_df <- data.frame()
context <- "hist2"
overwrite <- T

# Load in allResults
allResults <- readRDS(file = "./dev/benchmarks.rds")

# if defined allOverwrite, change overwrite to that

# Currently taken from testing scripts
benchmarkName <- "basic hist"
map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(bigData)
viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
binReportValue <- 'binWidth'

results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  )

# Print diff from benchmark based on context, name

results_df <- rbind(results_df, cbind(context, benchmarkName, summary(results)))



# Dates
benchmarkName <- "date hist"
map <- data.frame('id' = c('group', 'date', 'panel'), 'plotRef' = c('overlayVariable', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'DATE', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(data.dates)
viewport <- list('xMin'=min(df$date), 'xMax'=max(df$date))
binReportValue <- 'binWidth'
results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth=NULL, value='proportion', binReportValue, viewport)
)

# Print diff from benchmark based on context, name

results_df <- rbind(results_df, cbind(context, benchmarkName, summary(results)))


# Check against saved df with context = "hist"

# If overwrite = T, replace saved times
if (overwrite) {
  
  # Remove all data from current context
  allResults[allResults$context == context] <- NULL
  
  # Add new results and save
  allResults <- rbind(allResults,results_df)
  saveRDS(allResults, "./dev/benchmarks.rds")
}


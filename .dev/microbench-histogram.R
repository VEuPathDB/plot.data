## Microbenchmarking
# Histogram
context <- "hist3"
overwrite <- T

# Prepare dt
results_dt <- data.table()

# Load in allResults dt
allResults <- readRDS(file = "./dev/benchmarks.rds")

# if defined allOverwrite, change overwrite to that

# Currently taken from testing scripts
library(crayon)
name <- "basic hist"

map <- data.frame('id' = c('group', 'var', 'panel'), 'plotRef' = c('facetVariable2', 'xAxisVariable', 'facetVariable1'), 'dataType' = c('STRING', 'NUMBER', 'STRING'), stringsAsFactors=FALSE)
df <- as.data.frame(bigData)
viewport <- list('xMin'=min(bigData$var), 'xMax'=max(bigData$var))
binReportValue <- 'binWidth'

results <- microbenchmark::microbenchmark(
  histogram.dt(df, map, binWidth = NULL, value='count', binReportValue, viewport)
  ) %>% summary()

# Print diff from benchmark based on context, name
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
cat(paste0(context,", ",name, ": ") %+% red(results$median) %+% " in a block of text\n")


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

# Print diff from benchmark based on context, name
previousResult <- allResults[benchmarkContext == context & benchmarkName == name]
cat(paste0(context,", ",name, ": ") %+% red(results$median) %+% " in a block of text\n")


results_dt <- rbind(results_dt, cbind('benchmarkContext'=context, 'benchmarkName'=name, results))


# If overwrite == T, replace saved times
if (overwrite) {
  
  # Remove all data from current context
  allResults <- allResults[benchmarkContext != context]
  
  # Add new results and save
  allResults <- rbind(allResults,results_dt)
  saveRDS(allResults, "./dev/benchmarks.rds")
}


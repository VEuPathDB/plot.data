## Benchmarking helpers

# Print appropriately. In the future should make these more generalizable
compareToPrevious <- function(currentResult, previousResult, context, name) {
  if (NROW(previousResult) == 1) {
    if (currentResult$median < previousResult$median) {
      cat(paste0(context,", ",name, ": ") %+% cyan(currentResult$median - previousResult$median) %+% " milliseconds \n")
    } else {
      cat(paste0(context,", ",name, ": +") %+% red(currentResult$median - previousResult$median) %+% " milliseconds \n")
    }
  } else if(NROW(previousResult) == 0) {
    cat(paste0("No previous results for ", context, ", ", name, ". Median run time ",currentResult$median, " milliseconds \n")) 
  } else {
    cat("Benchmark test name and context not unique!")
  }
}
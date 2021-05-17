## Run all microbenchmark scripts

# Should we overwrite all data??
allOverwrite <- T

# get all files in .dev directory
allFiles <- list.files(path="./.dev/", include.dirs = F, recursive =F, full.names=T)
benchmarkingFiles <- allFiles[grepl("*microbench-.*[.]R$", allFiles)]

# Run all scripts - compare to saved data structure
for (i in seq_along(benchmarkingFiles)) {
  source(benchmarkingFiles[i], echo=F)
}

# Examine allResults for more info.


#
# you can run this in a R container...
# see ./test-container/README
#

# Handle plot.data environment (time zone, for example)
library(dotenv)
load_dot_env(file=".dev/.env")

# Helpful for development
library(devtools)
library(sloop)

# Load local library. If for ex we're already in the plot.data dir, will load the local plot.data
devtools::load_all()

# test
devtools::test()


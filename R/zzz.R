## On load, run the following
.onLoad <- function(libname, pkgname) {
  Sys.setenv(TZ='EST')
  packageStartupMessage("plot.data loaded. Happy plotting!")
  invisible()
}

# on.Unload() to remove side effects.
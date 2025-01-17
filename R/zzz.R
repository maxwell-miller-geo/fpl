# Make sure data.table is being used first
.onLoad <- function(libname, pkgname) {
  options(datatable.force = TRUE)
}

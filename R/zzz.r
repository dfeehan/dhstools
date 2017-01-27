## since we use c++ code, we have to do more cleanup
## when the package is unloaded; see
##    http://r-pkgs.had.co.nz/src.html
## (accessed 20150219) for more info
.onUnload <- function (libpath) {
      library.dynam.unload("dhstools", libpath)
}

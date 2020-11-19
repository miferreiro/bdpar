.onLoad <- function(libname, pkgname) {
  bdpar.Options$reset()
  bdpar.Options$configureLog()
  invisible()
}

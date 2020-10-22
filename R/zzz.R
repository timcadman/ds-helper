#' Initialization functions to do init actions
#' 
#' @param name of the library
#' @param pkgname package name
#' 
#' @importFrom utils globalVariables
#' 
#' @keywords internal
.onLoad <- function(libname, pkgname) { # nolint
  globalVariables("opals")
  invisible()
}
#' Initialization functions to do init actions
#'
#' @param libname path to library where the package is installed
#' @param pkgname package name
#'
#' @importFrom utils globalVariables
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) { # nolint
  globalVariables("opals")
  invisible()
}
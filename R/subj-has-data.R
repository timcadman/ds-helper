#' Identify subjects with available data
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This was an early version of \code{dh.defineCases()} which has in turn been deprecated. Please 
#' use \code{dsTidyverseClient::ds.filter()}
#' @keywords internal
#' @export
dh.subjHasData <- function() {
  lifecycle::deprecate_stop("1.6.0", "dh.subjHasData()", "dsTidyverseClient::ds.filter()")
}

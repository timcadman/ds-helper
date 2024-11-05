#' Group and subset a data frame 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This was an early version of dh.makeStrata, which has in turn been deprecated in favour of 
#'  \code{dsTidyverseClient::ds.arrange() |> dsTidyverseClient::ds.group_by() |> 
#' dsTidyverseClient::ds.slice()}.
#' @keywords internal
#' @export
dh.makeOutcome <- function(...) {
  lifecycle::deprecate_stop("1.6.0", "dh.makeOutcome()", details = "Please use the following functions 
                            from dsTidyverseClient: ds.arrange(), ds.group_by() and ds.slice()")
}

#' This was the original version of dh.meanByGroup. It is now defunct.
#'
#' @title dh.meanByAge
#' @template conns
#' @template df
#' @param outcome outcome variable in long format
#' @param age_var age in years
#' @param intervals table defining our age bands
#' @template checks
#'
#' @return Mean values for each unit of your age variable are returned
#'
#' @name dh.meanByAge-defunct
#' @usage dh.meanByAge(conns, df, outcome, age_var, intervals, checks)
#' @seealso \code{\link{dsHelper-deprecated}}
#' @keywords internal
NULL

#' @rdname dsHelper-defunct
#' @section \code{dh.meanByAge}:
#' For \code{dh.meanByAge}, use \code{\link{dh.meanByGroup}}.
#'
#' @export
dh.meanByAge <- function(...) {
    .Defunct(msg = "'dh.meanByAge' has been removed from this package. 
    Use dh.meanByAge instead. See help('Defunct')")
  }
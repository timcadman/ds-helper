#' This was the original version of dh.meanByGroup. It has now been deprecated.
#'
#' Whilst I always use this with 'age' as a grouping variable, it is a more
#' general function than that. I have thus renamed it to reflect that any
#' continuous grouping variable can be used.
#'
#' @template conns
#' @template df
#' @param outcome outcome variable in long format
#' @param age_var age in years
#' @param intervals table defining our age bands
#' @template checks
#'
#' @return Mean values for each unit of your age variable are returned
#'
#' @export
dh.meanByAge <- function(df = NULL, outcome = NULL, age_var = NULL, 
  intervals = NULL, conns = NULL, checks = FALSE) {
  
   .Deprecated("dh.meanByAge")
  message("dh.meanByAge will be defunct from version 1.0.0 onwards")

dh.meanByGroup(
  df = df, 
  outcome = outcome, 
  group_var = age_var, 
  conns = conns, 
  intervals = intervals, 
  checks = checks)

}
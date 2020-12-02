#' Check whether variables exist in dataframe
#'
#' Probably mostly for internal use, this function can be used at the start
#' of functions where users provide both vars and a dataframe to check that
#' the vars exist in the dataframe provided
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#'
#' @return None. Stops function if var(s) don't exist in one of more cohorts.
#'
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.colnames
#'
#' @noRd
dh.localProxy <- function(conns = conns, df) {
  dh.doesDfExist(conns, df)

  
}

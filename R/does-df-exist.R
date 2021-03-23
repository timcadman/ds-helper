#' Check whether dataframe exists in environment
#'
#' Again for internal use, this function can be used at the start of
#' functions where users provide a dataframe to check that the df exists
#' When I've got more time combine this with "cs.doVarsExist"
#'
#' @param conns connections to DataSHIELD backends
#' @param df opal dataframe
#'
#' @return None. Stops function if df doesn't exist in one of more cohorts.
#'
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.ls 
#'
#' @noRd
dh.doesDfExist <- function(conns = NULL, df) {
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  df_check <- names(conns) %>%
    map(~ (df %in% ds.ls(datasources = conns[.])[[1]][["objects.found"]]))

  names(df_check) <- names(conns)

  df_missing <- df_check %>% map(~ any(. == FALSE))

  if (any(unlist(df_missing) == TRUE)) {
    stop("Dataframe not present in one or more cohorts", call. = FALSE)
  }
}

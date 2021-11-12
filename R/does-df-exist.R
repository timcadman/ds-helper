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
#' @importFrom DSI datashield.connections_find
#' @importFrom dplyr %>%
#'
#' @noRd
dh.doesDfExist <- function(conns = NULL, df) {
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  objs <- datashield.aggregate(conns, call("lsDS", env.to.search = 1L))
  objs <- objs %>% map(~ .[["objects.found"]])

  df_check <- objs %>% map(~ df %in% .)

  df_missing <- df_check %>% map(~ any(. == FALSE))

  if (any(unlist(df_missing) == TRUE)) {
    stop("Dataframe not present in one or more cohorts", call. = FALSE)
  }
}

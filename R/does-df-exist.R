#' Check whether dataframe exists in environment
#'
#' Again for internal use, this function can be used at the start of
#' functions where users provide a dataframe to check that the df exists
#' When I've got more time combine this with "cs.doVarsExist"
#'
#' @param df opal dataframe
#' @param cohorts optional argument specifying which cohorts to use
#'
#' @return None. Stops function if df doesn't exist in one of more cohorts.
#'
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.ls
#'
#' @author Tim Cadman
#' @export
dh.doesDfExist <- function(df, cohorts = names(opals)) {
  df_check <- cohorts %>%
    map(~ (df %in% ds.ls(datasources = opals[.])[[1]][["objects.found"]]))

  names(df_check) <- cohorts

  df_missing <- df_check %>% map(~ any(. == FALSE))

  if (any(unlist(df_missing) == TRUE)) {
    
    stop("Dataframe not present in one or more cohorts", call. = FALSE)
  }
}

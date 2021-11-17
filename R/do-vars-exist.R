#' Check whether variables exist in dataframe
#'
#' Probably mostly for internal use, this function can be used at the start
#' of functions where users provide both vars and a dataframe to check that
#' the vars exist in the dataframe provided
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#' @param vars vector of variable names expected to be contained in dataframe
#'
#' @return None. Stops function if var(s) don't exist in one of more cohorts.
#'
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.colnames
#' @importFrom DSI datashield.connections_find
#'
#' @noRd
dh.doVarsExist <- function(df = NULL, vars = NULL, conns = NULL) {
  if (is.null(df)) {
   stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  allvars <- datashield.aggregate(conns, call("colnamesDS", df))

  var_check <- allvars %>% map(~ (vars %in% .))

  any_missing <- var_check %>% map(~ any(. == FALSE))

  if (any(unlist(any_missing) == TRUE)) {
    missing <- var_check %>%
      map(
        ~ paste0(
          vars[which(. == FALSE)],
          collapse = ", "
        )
      ) %>%
      unlist()

    stop(paste0(
      "Variable(s) specified in `vars` are not present in dataframe specified in `df`: ",
      paste0(missing, " (", names(missing), ")", collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

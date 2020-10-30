#' Check whether variables exist in dataframe
#'
#' Probably mostly for internal use, this function can be used at the start
#' of functions where users provide both vars and a dataframe to check that
#' the vars exist in the dataframe provided
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#' @param vars vector of variable names expected to be contained in dataframe
#' @param cohorts optional argument specifying which cohorts to use
#'
#' @return None. Stops function if var(s) don't exist in one of more cohorts.
#'
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.colnames
#'
#' @export
dh.doVarsExist <- function(conns = opals, df, vars, cohorts) {
  if (missing(cohorts)) {
    cohorts <- names(conns)
  }

  allvars <- ds.colnames(x = df, datasources = conns[cohorts])

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
      "Variable(s) not present in the data frame: ",
      paste0(missing, " (", names(missing), ")", collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

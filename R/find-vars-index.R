#' Return return indices of column names in server-side dataframe
#'
#' Some DataSHIELD functions require column indices as parameters. This is
#' hqighly susceptable to breaking as changes in code will change the order of
#' variables. This function allows you to specify the names of columns and
#' returns their indices.
#'
#' @template conns
#' @template df
#' @param vars Character vector of columns within `df` for which to return the
#' indices.
#' @template checks
#'
#' @return Client-side list of indices corresponding to `vars`, where the length
#' of the list corresponds to the number of cohorts included in `conns`.
#'
#' @importFrom dsBaseClient ds.colnames
#' @importFrom purrr pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.findVarsIndex <- function(df = NULL, vars = NULL, conns = NULL,
                             checks = TRUE) {
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  cols <- datashield.aggregate(conns, call("colnamesDS", df))

  out <- cols %>% map(~ which(.x %in% vars == TRUE))

  names(out) <- names(conns)

  return(out)
}

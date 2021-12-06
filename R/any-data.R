#' Describe whether variables are completely missing for each cohort
#'
#' When performing a Study Level Meta Analysis of coefficients, it
#' is usually necessary that all cohorts have some data on all variables in the
#' model. Manually identifying which cohorts have some data on required
#' variables is time-consuming and at risk of error. This function automatically
#' generates a look-up table with this information.
#'
#' @template df
#' @param vars Character vector of columns within `df`.
#' @template checks
#' @template conns
#'
#' @return Client-side tibble containing columns for each variable and rows
#' for each cohort indicating if the variable is not completely missing.
#'
#' @family descriptive functions
#'
#' @importFrom dplyr %>% mutate select everything
#' @importFrom dsBaseClient ds.colnames ds.numNA ds.length
#' @importFrom DSI datashield.connections_find
#' @importFrom tibble as_tibble
#'
#' @export
dh.anyData <- function(df = NULL, vars = NULL, conns = NULL, checks = TRUE) {
  if (is.null(df)) {
    stop("`df` must not be NULL.")
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (is.null(vars)) {
    fun_vars <- unique(unlist(ds.colnames(df, datasources = conns)))
  } else {
    fun_vars <- vars
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  # get the lengths
  lengths <- unlist(ds.length(paste0(df, "$", fun_vars[1]), type = "s", datasources = conns))

  list_na <- lapply(fun_vars, function(x) {
    numNa <- unlist(ds.numNA(paste0(df, "$", x), datasources = conns))
    numNa != lengths
  })

  names(list_na) <- fun_vars
  out <- bind_rows(list_na, .id = "variable")

  return(out)
}

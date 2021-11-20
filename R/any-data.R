#' Not completely missing variables
#'
#' When performing a Study Level Meta Analysis of coefficients linear models (for example),
#' the intention should be that a consistent set of confounding variables is available
#' for each cohort and used during model fitting.
#' However it is not strictly necessary to have all confounding variables available in all
#' cohorts in order to generate a coefficient representing the relationship between exposure
#' and outcome.
#' Assume that some cohorts have some completely empty confounding variables. In this scenario
#' it is useful to be able to specify a model to fit on all cohorts and use a look up table
#' to understand if there are any cohorts that lack a particular confounder. This look up
#' table can be used to automatically exclude completely missing confounders without having
#' to do this by hand. This is particularly useful if you are looking at a large number of
#' combinations of exposures, outcomes and confounders.
#' The purpose of this function is to generate this look up table automatically.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df server side dataframe
#' @param vars vector of server side variable names within df
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#'
#' @return a dataframe with columns for each variable and rows for each cohort
#' indicating if the variable is not completely missing
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

  if(checks == TRUE){
  .isDefined(obj = df, datasources = conns)
}

  if (is.null(vars)) {
    fun_vars <- unique(unlist(ds.colnames(df, datasources = conns)))
  } else {
    fun_vars <- vars
  }

  if(checks == TRUE){
  paste0(df, "$", vars) %>% map(~.isDefined(obj = .x, datasources = conns))
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

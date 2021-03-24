#' Class discrepancy
#'
#' Either due to using ds.dataFrameFill, or because of mistakes in uploading
#' data, some cohorts may have variables uploaded as a different class to other
#' cohorts. This can create problems, e.g. when using ds.summary. This function
#' produces a table comparing the class of multiple variables.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe (optional). If vars is not
#'      provided all variables will be included.
#'
#' @return a tibble with columns for (i) variable, (ii) discrepancy (y/n) and
#'        columns for each cohort indicating the class of the variable
#'
#' @importFrom purrr map_df
#' @importFrom dplyr %>% mutate select everything
#' @importFrom dsBaseClient ds.class ds.colnames 
#' 
#' @export
dh.classDiscrepancy <- function(df, vars = NULL, conns = NULL) {
  
  . <- variable <- discrepancy <- NULL

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  dh.doesDfExist(conns, df)

  if (is.null(vars)) {
    fun_vars <- unique(unlist(ds.colnames(df, datasources = conns)))
  } else {
    fun_vars <- vars
  }

  out <- paste0(df, "$", fun_vars) %>%
    map_df(function(x) {
      ds.class(x, datasources = conns)
    }) %>%
    mutate(
      discrepancy = apply(., 1, function(x) {
        ifelse(length(unique(x)) == 1, "no", "yes")
      }),
      variable = fun_vars
    ) %>%
    select(variable, discrepancy, everything())

  return(out)
}

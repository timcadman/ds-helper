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
#' @importFrom DSI datashield.connections_find
#'
#' @export
# nolint
dh.classDiscrepancy <- function(df = NULL, vars = NULL, conns = NULL) {
  . <- variable <- discrepancy <- NULL

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doesDfExist(conns, df)
  
  if (is.null(vars)) {

    fun_vars <- unique(unlist(datashield.aggregate(conns, call("colnamesDS", df))))
    
  } else {
    dh.doVarsExist(df, vars, conns)
    fun_vars <- vars
  }

  ## ---- Get variable classes -------------------------------------------------
  classes <- paste0(df, "$", fun_vars) %>%
    map_df(function(x){
      calltext <- call('classDS', x)
      datashield.aggregate(conns, calltext)
    })
  
  ## ---- Create column indicating whether there is a discrepancy --------------
  out <- classes %>%
    mutate(
      discrepancy = pmap_chr(., function(...){
        dots <- list(...)
        ifelse(length(unique(dots)) == 1, "no", "yes") 
      }),
      variable = fun_vars) %>%
    select(variable, discrepancy, everything())

  return(out)
}

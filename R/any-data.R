#' Describe whether variables are completely missing for cohort(s)
#'
#' When performing a Study Level Meta Analysis of coefficients, it is usually 
#' necessary that all cohorts have some data on all variables in the model. 
#' Manually identifying which cohorts have some data on required variables is 
#' time-consuming and at risk of error. This function automatically generates a 
#' look-up table with this information.
#'
#' @param conns DataSHIELD connections object.
#' @param df server side dataframe
#' @param vars vector of server side variable names within df
#'
#' @return a local dataframe with columns for each variable and rows for each 
#' cohort
#' indicating if the variable is not completely missing
#'
#' @importFrom dplyr %>% mutate select everything
#' @importFrom dsBaseClient ds.colnames ds.numNA ds.length
#' @importFrom DSI datashield.connections_find
#' @importFrom tibble as_tibble
#'
#' @export
dh.anyData <- function(df = NULL, vars = NULL, conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variables")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doesDfExist(conns, df)

  if (is.null(vars)) {
    fun_vars <- unique(unlist(ds.colnames(df, datasources = conns)))
  } else {
    fun_vars <- vars
  }

  dh.doVarsExist(df = df, vars = vars, conns = conns)

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

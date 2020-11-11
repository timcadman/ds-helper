#' Not completely missing variables
#'
#' When performing a Study Level Meta Analysis, the intention should be that a consistent set of confounding variables for each cohort.
#' However it is not strictly necessary to have all confounding variables available in all cohorts. Assume that some cohorts
#' have completely empty confounding variables. In this scenario it is useful
#' to be able to specify a model to fit on all cohorts and use a look up table to understand if there are any cohorts that lack a
#' particular confounder. The purpose of this function is to generate this look up table automatically.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df sever side dataframe
#'
#' @return a dataframe with columns for each variable and rows for each cohort indicating if the variable is not completely missing
#'
#' @importFrom dplyr %>% mutate select everything
#' @importFrom dsBaseClient ds.colnames ds.numNA ds.length
#'
#' @export
dh.notCompletelyMissing <- function(conns = opals, df) {
  . <- variable <- discrepancy <- NULL

  dh.doesDfExist(conns, df)
  fun_vars_all <- ds.colnames(df, datasources = conns)
  fun_vars <- fun_vars_all[[1]]
  
  
  if(!all(sapply(fun_vars_all, identical, fun_vars_all[[1]]))){stop("All columns not found in all cohorts, please investigate with dh.ClassDiscrepancy", call. = FALSE)}
  
  # get the lengths
  lengths = unlist(ds.length(paste0(df,"$",fun_vars[1]), type = "s"))
  
  list_na <- lapply(fun_vars, function(x){
    numNa = unlist(ds.numNA(paste0(df,"$",x)))
    numNa != lengths
  })
  
  out <- as.data.frame(do.call(cbind, list_na))
  colnames(out) <- fun_vars
  
  return(out)
}

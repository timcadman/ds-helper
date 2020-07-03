#' Class descrepancy
#' 
#' Either due to using ds.dataFrameFill, or because of mistakes in uploading 
#' data, some cohorts may have variables uploaded as a different class to other
#' cohorts. This can create problems, e.g. when using ds.summary. This function
#' produces a table comparing the class of multiple variables.
#'
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe (optional). If vars is not 
#'      provided all variables will be included.
#'  
#' @return a tibble with columns for (i) variable, (ii) discrepancy (y/n) and
#'        columns for each cohort indicating the class of the variable
#'        
#' @importFrom purrr map_df
#' @importFrom dplyr %>% mutate select        
#' @importFrom dsBaseClient ds.class ds.colnames
#' 
#' @export   
cs.classDescrepancy <- function(df, vars = NULL){

  cs.doesDfExist(df)
  
    if(is.null(vars)){
    
    fun_vars <- ds.colnames(df)[[1]]
    
  } else{
    
    fun_vars <- vars
    
  }
  
  out <- paste0(df, "$", fun_vars) %>%
    map_df(ds.class) %>%
    mutate(discrepancy = apply(., 1, function(x){
      ifelse(length(unique(x)) == 1, "no", "yes")}),
      variable = fun_vars) %>%
    select(variable, discrepancy, everything())
  
  return(out)
}
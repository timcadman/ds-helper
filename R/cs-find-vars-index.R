#' Return return indices of columns in opal dataframe
#' 
#' Some ds functions ask you to provide the index of variables as arguments.
#' This is highly susceptable to breaking! This is a simple function that will
#' return the index of variables for a given cohort. It is usefully used in 
#' combination the "keep.cols" argument of ds.dataFrameSubset.
#' 
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe
#' @param cohorts cohort that you want to find indices for
#' 
#' @return list of indices where length of list is number of cohorts provided
#' 
#' @importFrom dsBaseClient ds.colnames
#' @importFrom stringr str_detect
#' @importFrom purrr map_dbl
#' 
#' @export
cs.findVarsIndex <- function(df, vars, cohorts){
  
  varcheck <- cs.doVarsExist(df, vars)
  
  if(varcheck[[1]] == FALSE){
    
    stop(paste0(
      "The following variable(s) are not present in the data frame: ", 
      paste0(varcheck[[2]], collapse = ", ")), call. = FALSE
    )
    
  } else{

ref_tab <- tibble(
  var = rep(vars, length(names(opals))),
  cohort = rep(names(opals), each = length(vars))
  )    
   
tmp <- ref_tab %>%
  pmap(
    function(var, cohort){
    
    which(
      str_detect(
        ds.colnames(df, datasources = opals[cohort])[[1]], 
        paste0("\\b", var, "\\b")) == TRUE)
  })

out <- split(unlist(tmp), ceiling(seq_along(test2) / length(vars)))
names(out) <- names(opals)

return(out)

  }
}
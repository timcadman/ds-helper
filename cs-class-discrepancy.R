################################################################################
## Project: ds-cs-functions   
## Script purpose: Identify if the class of variables differs between cohorts
## Date: 5th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# Either due to using ds.dataFrameFill, or because of mistakes in uploading 
# data, some cohorts may have variables uploaded as a different class to other
# cohorts. This can create problems, e.g. when using ds.summary. This function
# produces a table comparing the class of multiple variables.
#
# Arguments:
#
# df = dataframe
# vars = vector of variable names in dataframe (optional). If vars is not 
#        provided all variables will be included.
#
# Value: a tibble with columns for (i) variable, (ii) discrepancy (y/n) and
#        columns for each cohort indicating the class of the variable

require(tidyr)
require(dplyr)
require(dsBaseClient)
require(purrr)

cs.classDescrepancy <- function(df, vars = NULL){
  
  if(is.null(vars)){
    
    vars <- ds.colnames(df)[[1]]
    
  } else{
    
    vars <- vars
    
  }
  
  out <- paste0(df, "$", vars) %>%
    map_df(ds.class) %>%
    mutate(discrepancy = apply(., 1, function(x){
      ifelse(length(unique(x)) == 1, "no", "yes")}),
      variable = vars) %>%
    select(variable, discrepancy, everything())
  
  return(out)
}
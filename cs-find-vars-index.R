################################################################################
## Project: ds-cs-functions 
## Script purpose: Return index of variables  
## Date: 5th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# Some ds functions ask you to provide the index of variables as arguments.
# This is highly susceptable to breaking! This is a simple function that will
# return the index of variables for a given cohort. It is usefully used in 
# combination the "keep.cols" argument of ds.dataFrameSubset.
#
# Arguments:
# 
# df = a dataframe
# vars = vector of variables
# cohorts = cohort that you want to find index for
#
# value = vector of indexes

require(dsBaseClient)
require(purrr)
require(stringr)

cs.findVarsIndex <- function(df, vars, cohorts){
  
   map_dbl(vars,
      
      ~which(
        str_detect(
          ds.colnames(df, datasources = opals[cohorts])[[1]], 
          paste0("\\b", 
                 .x,
                 "\\b")) == TRUE)
    )  
  
}
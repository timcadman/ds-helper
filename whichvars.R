################################################################################
## Project: ds-csfunctions
## Script purpose: Return indices of variables
## Date: 28th May 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################  

# To select a subset of variables within a dataframe you can use the
# "keep.cols" argument in ds.dataFrameSubset. However at present you can only
# provide indices of columns rather than their names. This is a simple function
# to extract the indices of variables from a dataframe based on a vector of
# variable names.

whichVars <- function(df, vars){

# arguments:
#
# df = name of dataframe
# vars = vector of variable names
#
# value = a list with the indices for each cohort, where number of list elements
#         equals the number of cohorts.  
    
  out <- lapply(names(opals), function(x){
    
    sapply(vars, function(y){
      
      which(
        str_detect(
          ds.colnames(df, datasources = opals[x])[[1]], y) == TRUE)
    })  
  })
  
  names(out) <- names(opals)
  
  return(out)
}
################################################################################
## Project: ds-common
## Script purpose: Shorten the process of subsetting a dataframe
## Date: 27th May 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# At some point in the analysis you will want to subset your dataset to contain
# only subjects meeting some criteria, e.g. data on at least one exposure and
# one outcome. This function speeds things up a bit. You can give it as an 
# input a list of variable names, and it will produce two variables: (i) a 
# variable indicating for each subject how many of these variables they have
# non-missing data for, and (ii) a variable stating whether subjects have any
# non-missing data on the variables that were provided.
#
# NOTE: YOU NEED TO ENSURE THAT YOU DON'T INCLUDE BINARY VARIABLES WHERE THE 
# LOWER VALUE IS CODED AS 0. IF SO THESE WILL BE MISTAKENLY CLASSED AS MISSING.
# I WILL SORT THIS PROBLEM OUT AT SOME POINT.
#
# Arguments:
#
# df = Name of the dataframe containing the variables
# vars = Vector of variable names
# newvar = label which forms the suffix for the two created variables
#
# value = none. New variables created within the opal environment.


df = "bmi_poc"
vars = exp.vars
newvar = "exposure"

## ---- Now create dataframe indicating which are available in each cohort -----
anyVarExists <- function(df, vars, newvar){
  
  # First we convert all the provided variables for numeric. This is because we
  # need to use ds.Boole to compare them to 0. These are just numeric copies of 
  # the variables we will end up keeping.
  
  sapply(vars, function(x){
    
    ds.asNumeric(paste0(df, "$", x), newobj = paste0(x, "_num"))  
    
  })
  
  
  # Now we create a vector of the names of these numeric variables
  
  vars_num <- paste0(vars, "_num")  
  
  # Now we create new variables indicating whether or not there are values >
  # 0 on that value. 
  
  sapply(vars_num, function(x){
    
    ds.Boole(V1 = x, V2 = "0", Boolean.operator = ">", na.assign = 0, 
             newobj = paste0(x, "_yn"))
    
  })
  
  # Now we count how many non-missing variables there are for each subject
  ds.make(toAssign=paste0(paste0(vars_num, "_yn"), collapse = "+"), 
          newobj = paste0("n_", newvar))
  
  # Finally we create another variable which states whether there are any non
  # missing variables for that subject
  ds.Boole(V1 = paste0("n_", newvar), V2 = "0", Boolean.operator = ">", na.assign = 0, 
           newobj = paste0("any_", newvar))
  
}
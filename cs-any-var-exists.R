################################################################################
## Project: ds-cs-functions
## Script purpose: Shorten the process of subsetting a dataframe
## Date: 27th May 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# At some point in the analysis you will want to subset your dataset to contain
# only subjects meeting some criteria, e.g. data on at least one exposure and
# one outcome. This function speeds things up by indicating whether a subject
# has any non-missing values for a given set of variables.
#
# Arguments:
#
# df = Name of the dataframe containing the variables
# vars = Vector of variable names
# new_label = label which forms the suffix for the two created variables
#
# value = none. New variables created within the opal environment.

require(dsBaseClient)

cs.anyVarExists <- function(df, vars, new_label){
  
## ---- Convert to numeric -----------------------------------------------------
  
# We do this because we will need to use ds.Boole to compare them to 0. These 
# are just numeric copies of the variables we will end up keeping.

sapply(vars, function(x){
  
  ds.asNumeric(paste0(df, "$", x), newobj = paste0(x, "_num"))  
  
})
  

## ---- Create a vector of these numeric variable names ------------------------
vars_num <- paste0(vars, "_num")  


## ---- Now evaluate whether there are values >= 0 -----------------------------
  
# If a value is NA it will return NA. 
sapply(vars_num, function(x){
  
  ds.Boole(V1 = x, 
           V2 = "0",
           Boolean.operator = ">=", 
           na.assign = 0, 
           newobj = paste0(x, "_yn"))
  
})


## ---- Count number of non-missing variables for each subject -----------------
ds.make(
  toAssign = paste0(paste0(vars_num, "_yn"), collapse = "+"), 
  newobj = paste0("n_", new_label)
  )


## ---- Create final variable indicating if there are any non-missing values ---
ds.Boole(
  V1 = paste0("n_", new_label), 
  V2 = "0", 
  Boolean.operator = ">", 
  na.assign = 0, 
  newobj = paste0("any_", new_label))
  
}
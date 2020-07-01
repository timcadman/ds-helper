################################################################################
## Project: ds-cs-functions
## Script purpose: Automate the process of renaming variables
## Date: 5th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################      

# At some point you're likely to want to rename variables. This function 
# automates the process within DataSHIELD. 
#
# Arguments:
#
# df = a dataframe containing the variables to be renamed
# names = a dateframe with two columns: "old_name" and "new_name" which contain
#         the current name of the variable and the new name you want it to have
# new_df_name = Name of new opal object holding these variables (optional). If
#               left blank the a new object is created overwriting the original
#               dataframe

require(dsBaseClient)
require(purrr)

cs.renameVars <- function(df, names, new_df_name = NULL){
  
  names %>%
    pmap(function(oldvar, newvar){
      
      ds.assign(
        toAssign = paste0(df, "$", names$oldvar),
        newobj = newvar
      )  
    })
  
  if(is.null(new_df_name)){
    
    out_df <- df
    
  } else{
    
    out_df <- new_df_name
  }
  
  names(opals) %>%
    map(
      ~ds.dataFrame(
        x = names$newvar,
        newobj = out_df,
        datasources = opals[.]
      )
    )
}
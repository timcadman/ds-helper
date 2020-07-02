#' Rename multiple variables at once
#' 
#' This function allows you to rename multiple variable from a dataframe. At the
#' moment it doesn't "rename" as such, it creates duplicate variables with the
#' new names. I've left it like this to keep in the spirit of ds/opal set up
#' by not automating the deletion of variables.
#' 
#' @param df dataframe
#' @param names a dataframe or tibble containing two columns: "oldvar" (existing
#'              variable name), "newvar" (new variable name). Each row 
#'              corresponds to one variable you want to rename
         #'              
#' @return None. The new variables are added to the df specified
#' 
#' @importFrom dsBaseClient ds.assign ds.dataFrame
#' @importFrom purrr map pmap
#' 
#' @export                                                       
cs.renameVars <- function(df, names){
  
  names %>%
    pmap(function(oldvar, newvar, ...){
      
      ds.assign(
        toAssign = paste0(df, "$", oldvar),
        newobj = newvar
      )  
    })

names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c(df, names$newvar),
      newobj = df,
      datasources = opals[.])
    )

old_new %>% pull(newvar) %>% cs.rmLots %>% invisible

}
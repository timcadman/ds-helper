#' Check whether dataframe exists in environment
#' 
#' Again for internal use, this function can be used at the start of 
#' functions where users provide a dataframe to check that the df exists
#' When I've got more time combine this with "cs.doVarsExist"
#'
#' @param df opal dataframe
#'  
#' @return None. Stops function if df doesn't exist in one of more cohorts.
#'        
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.ls
#' 
#' @export 
dh.doesDfExist <- function(df){

df_check <-  names(opals) %>%
  map(~(df %in% ds.ls(datasources = opals[.][[1]])))

names(df_check) <- names(opals)

df_missing <- df_check %>% map(~any(. == FALSE)) 

if(any(unlist(df_missing) == TRUE)){
  
  missing <- df_check %>%
    map(
      ~paste0(
        vars[which(. == FALSE)], 
        collapse = ", ")) %>%
    unlist()
  
  stop(paste0(
    "Dataframe(s) not present in environment: ", 
    paste0(missing, " (", names(missing), ")", collapse = ", ")), 
    call. = FALSE
  )
}
}
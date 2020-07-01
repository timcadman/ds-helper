#' Check whether variables exist in dataframe
#' 
#' Probably mostly for internal use, this function can be used at the start
#' of functions where users provide both vars and a dataframe to check that 
#' the vars exist in the dataframe provided
#'
#' @param df dataframe
#' @param vars vector of variable names expected to be contained in dataframe
#'  
#' @return a list with two elements: (i) TRUE/FALSE depending on whether all 
#'         provided variables are in the data frame, (ii) vector of variables
#'         not present in dataframe (NULL if first list element == TRUE)
#'        
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.class ds.colnames
#' @importFrom stringr str_detect
#' 
#' @export 
cs.doVarsExist <- function(df, vars){
  
  if(length(vars) == 0){
    
    stop("Error: no variables provided", call. = FALSE)
    
  }
  
  allvars <- ds.colnames(df)[[1]]
  
  what_exists <- map(vars, 
                     ~any(str_detect(allvars, paste0("\\b", .x, "\\b")) == TRUE)
  )
  
  names(what_exists) <- vars
  
  out <- list(allvars = NULL, whichvars = NULL)
  
  out[[1]] <- all(what_exists == TRUE)  
  
  if(out[[1]] == TRUE){
    
    out[[2]] <- NULL
    
  } else if(out[[1]] == FALSE){
    
    out[[2]] <- names(what_exists)[which(what_exists == FALSE)]
    
  }
  
  return(out)
}
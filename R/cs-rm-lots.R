#' Remove multiple objects from ds environment
#' 
#' This is a very simple wrapper around ds.rm to allow you to remove more than 
#' one object at a time.
#' 
#' @param vars objects that you want to remove
#' 
#' @return None. Objects removed from ds environment
#' 
#' @importFrom purrr map
#' @importFrom dsBaseClient ds.rm
#' @importFrom dplyr %>%
#' 
#' @export
cs.rmLots <- function(vars){
  
  vars %>% map(ds.rm)
  
}
#' Tidy up the server environment
#' 
#' This is a very simple wrapper around ds.rm to allow you to remove more than 
#' one object at a time.
#' 
#' @param obj objects that you want to either keep or remove
#' @param type either "remove" to remove the listed objects of "keep" to keep
#'             the listed objects and remove everything else.
#' 
#' @return None. Objects removed from ds environment
#' 
#' @importFrom purrr map imap
#' @importFrom dsBaseClient ds.rm
#' @importFrom dplyr %>%
#' 
#' @export
cs.tidyEnv <- function(obj, type = "remove"){
  
  if(type == "remove"){
  
obj %>% map(ds.rm)    

    } else if(type == "keep"){
    
objects <- names(opals) %>%
  map(
    ~ds.ls(datasources = opals[.])[[1]]
    )

vars <- seq(1 : length(names(opals))) %>%
  map(
    ~objects[[.]][objects[[.]] %in% obj == FALSE]
  )

names(vars) <- names(opals)

## Check no objects to removed have character length >20
obj_lengths <- vars %>% 
  map(~nchar(.)) %>%
  map(~any(. > 20)) %>%
  unlist() %>%
  any(. > 20)

if(obj_lengths == TRUE){
  stop("You are attempting to remove objects with name(s) longer than 20 
characters. DS does not permit this due to risk of malicious code. Amend your 
       script so that your objects have shorter names", call. = FALSE)
}

vars %>% imap(
  ~ds.rm(x.name = .x, datasources = opals[.y])
)

    }

}

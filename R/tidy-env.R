#' Tidy up the server environment
#'
#' This is a very simple wrapper around ds.rm to allow you to remove more than
#' one object at a time.
#'
#' @param conns connections object to DataSHIELD backends
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
dh.tidyEnv <- function(conns = opals, obj, type = "remove") {
  . <- NULL
  
  if (type == "remove") {
    obj %>% map(ds.rm, datasources = conns)
  } else if (type == "keep") {
    objects <- names(conns) %>%
      map(
        ~ ds.ls(datasources = conns[.])[[1]][[2]]
      )

    vars <- seq(1:length(names(conns))) %>%
      map(
        ~ objects[[.]][objects[[.]] %in% obj == FALSE]
      )

    names(vars) <- names(conns)

    ## Check no objects to removed have character length >20
    obj_lengths <- vars %>%
      map(~ nchar(.)) %>%
      map(~ any(. > 20)) %>%
      unlist() %>%
      any(. > 20)

    if (obj_lengths == TRUE) {
      stop("You are attempting to remove objects with name(s) longer than 20 
characters. DS does not permit this due to risk of malicious code. Amend your 
       script so that your objects have shorter names", call. = FALSE)
    }

    vars_tibble <- vars %>%
      map(~ as_tibble(.)) %>%
      imap(~ mutate(., cohort = .y)) %>%
      bind_rows()

    vars_tibble %>% pmap(function(cohort, value) {
      ds.rm(x.name = value, datasources = conns[cohort])
    })
  }
}

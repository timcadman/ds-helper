#' Remove multiple objects from the serverside environment
#'
#' This is a wrapper around ds.rm to allow you to remove multiple objects
#' in one call.
#'
#' @param conns DataSHIELD connections object.
#' @param obj Server-side objects that you want to either keep or remove.
#' @param type Either "remove" to remove objects specified in `obj` or "keep" to
#' keep objects specified in `obj` and remove everything else.
#'
#' @return None. Objects are removed from the server-side environnment.
#'
#' @importFrom purrr map imap
#' @importFrom dsBaseClient ds.rm ds.ls
#' @importFrom dplyr %>%
#' @importFrom DSI datashield.connections_find
#'
#' @family data manipulation functions
#'
#' @export
dh.tidyEnv <- function(obj = NULL, type = NULL, conns = NULL) {
  . <- NULL

  if (is.null(obj)) {
    stop("`obj` must not be NULL.", call. = FALSE)
  }

  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  }

  type <- arg_match(type, c("remove", "keep"))

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (type == "remove") {
    obj %>% map(ds.rm, datasources = conns)
  } else if (type == "keep") {
    objects <- names(conns) %>%
      map(
        ~ ds.ls(datasources = conns[.])[[1]][[2]]
      )

    vars <- seq(1:length(names(conns))) %>% # nolint
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
      stop("You are attempting to remove objects with name(s) longer than 20 characters. DS does not permit this
           due to risk of malicious code. Amend your script so that your objects have shorter names", call. = FALSE)
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

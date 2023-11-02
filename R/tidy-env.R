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
  . <- value <- NULL

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

    ## Check no objects to removed have character length >20
    obj_lengths <- tibble(
      obj = obj,
      length = obj %>% map_int(nchar)
    )

    obj_valid <- obj_lengths %>%
      dplyr::filter(length < 20) %>%
      pull(obj)

    obj_not_valid <- obj_lengths %>%
      dplyr::filter(length >= 20)

    if (nrow(obj_not_valid > 0)) {
      warning(paste0("You are attempting to remove objects with name(s) longer than 20 characters. DS does not permit this
           due to risk of malicious code. These objects have not been removed: \n\n", as.character(obj_not_valid$value)), call. = FALSE)
    }

    obj_valid %>%
      map(
        ~ ds.rm(x.names = .x, datasources = conns)
      )
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

    vars_tibble <- vars %>%
      map(~ as_tibble(.)) %>%
      imap(~ mutate(., cohort = .y)) %>%
      bind_rows() %>%
      mutate(length = value %>% map_int(nchar))

    obj_valid <- vars_tibble %>%
      dplyr::filter(length < 20)

    obj_not_valid <- vars_tibble %>%
      dplyr::filter(length >= 20)

    if (nrow(obj_not_valid > 0)) {
      warning(paste0("You are attempting to remove objects with name(s) longer than 20 characters. DS does not permit this
           due to risk of malicious code. These objects have not been removed.", as.character(obj_not_valid$value)), call. = FALSE)
    }

    obj_valid %>% pmap(function(cohort, value, ...) {
      ds.rm(x.names = value, datasources = conns[cohort])
    })
  }
}

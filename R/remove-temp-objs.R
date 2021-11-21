#' Checks the server environment for new objects generated and removes them
#'
#' Often with dsHelper functions we will create temporary objects which we want
#' to remove. This function checks the difference between the output of `ds.ls`
#' called at different points in the analysis. It removes objects present in
#' the second set which were not present in the first.
#'
#' @param start_objs Output from ds.ls run at the start of a function
#' @param others_to_keep Optionally, additional variables not to remove, e.g.
#' you might not want to remove an output object you have created.
#' @template conns
#'
#' @importFrom dsBaseClient ds.ls
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr map pmap imap
#'
#' @noRd
.removeTempObjs <- function(start_objs, others_to_keep, conns) {
  end_objs <- ds.ls(datasources = conns)

  remove_ref <- tibble(
    start_clean = start_objs %>% map(~ .x$objects.found),
    end_clean = end_objs %>% map(~ .x$objects.found)
  )

  to_keep <- remove_ref %>%
    pmap(function(start_clean, end_clean) {
      start_clean[start_clean %in% end_clean == TRUE]
    })

  to_keep <- to_keep %>% map(function(x) {
    c(x, others_to_keep)
  })

  to_keep %>%
    imap(
      ~ dh.tidyEnv(obj = .x, type = "keep", conns = conns[.y])
    )
}

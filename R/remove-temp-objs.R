#' Often with dsHelper functions we will create temporary objects
#' on the server which we want to remove. This function
#' checks the difference between two sets of objects and removes
#' those not present in the first
#'
#' @param start_objs output from ds.ls run at the start of a function
#' @param others_to_keep optional. Additional variables not to remove,
#' e.g. you might not want to remove an output object you have created.
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

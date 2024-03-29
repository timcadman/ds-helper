#' Describes the class of one or more variables across cohorts and indicates
#' differences
#'
#' Either through using ds.dataFrameFill, or due to mistakes in uploading
#' data, the same variable may have a different class across cohorts. This can
#' create problems as many DataSHIELD functions require the input variable to
#' have the same class in all studies. This function produces a tibble showing
#' the class of each variable.
#'
#' @template conns
#' @template df
#' @param vars Optionally, a character vector specifying columns within `df` to
#' describe. If NULL all variables will be included.
#' @template checks
#'
#' @return A tibble with columns for each variable and rows for each cohort
#' describing the class of the variable, with an additional column 'discrepancy'
#' summarising whether there are differences between cohorts.
#'
#' @family descriptive functions
#'
#' @importFrom purrr map_df
#' @importFrom dplyr %>% mutate select everything
#' @importFrom dsBaseClient ds.class ds.colnames
#' @importFrom DSI datashield.connections_find
#'
#' @export
# nolint
dh.classDiscrepancy <- function(df = NULL, vars = NULL, conns = NULL, checks = TRUE) {
  . <- variable <- discrepancy <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (is.null(vars)) {
    fun_vars <- unique(unlist(datashield.aggregate(conns, call("colnamesDS", df))))
  } else {
    fun_vars <- vars
  }

  if (checks == TRUE) {
    .isDefined(df = df, conns = conns)
  }

  ## ---- Get variable classes -------------------------------------------------
  classes <- paste0(df, "$", fun_vars) %>%
    map_df(function(x) {
      calltext <- call("classDS", x)
      datashield.aggregate(conns, calltext) %>%
        lapply(function(x) {
          paste(x, collapse = ",")
        })
    })

  ## ---- Create column indicating whether there is a discrepancy --------------
  out <- classes %>%
    mutate(
      discrepancy = pmap_chr(., function(...) {
        dots <- list(...)
        ifelse(length(unique(dots)) == 1, "no", "yes")
      }),
      variable = fun_vars
    ) %>%
    select(variable, discrepancy, everything())

  return(out)
}

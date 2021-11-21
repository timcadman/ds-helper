#' Rename one or more columns within a serverside data frame
#'
#' This function is an analogue of `dplyr::rename` which allows you to rename 
#' columns of a serverside data frame.
#'
#' @template conns
#' @template df
#' @param current_names A character vector of columns within `df` to rename.
#' @param new_names A character vector giving the new names for the columns 
#' described in `current_names`.
#' @template checks
#' @return None. The new variables are added to the df specified
#'
#' @importFrom dsBaseClient ds.assign ds.dataFrame
#' @importFrom purrr map pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.renameVars <- function(df = NULL, current_names = NULL, new_names,
                          conns = NULL, checks = TRUE) {
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(current_names)) {
    stop("`current_names` must not be NULL.", call. = FALSE)
  }

  if (is.null(new_names)) {
    stop("`new_names` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  names <- NULL

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  if (length(current_names) != length(new_names)) {
    stop("Length of `current_names` must equal length of `new_names`.
    Please check input vectors", call. = FALSE)
  }

  names <- list(oldvar = current_names, newvar = new_names)

  names %>%
    pmap(function(oldvar, newvar, ...) {
      ds.assign(
        toAssign = paste0(df, "$", oldvar),
        newobj = newvar,
        datasources = conns
      )
    })

  ds.dataFrame(
    x = c(df, names$newvar),
    newobj = df,
    datasources = conns
  )

  dh.dropCols(
    df = df,
    vars = current_names,
    new_df_name = df,
    conns = conns,
    checks = FALSE
  )

  dh.tidyEnv(
    obj = new_names,
    conns = conns
  ) %>%
    invisible()
}

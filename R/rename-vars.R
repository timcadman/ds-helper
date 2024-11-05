#' Rename one or more columns within a serverside data frame
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' This function was an analogue of \code{dplyr::rename()}. It was deprecated because you can now use
#' \code{dsTidyverseClient::ds.rename()} which is much quicker and more flexible.
#'
#' @template conns
#' @template df
#' @param current_names Character vector of columns within `df` to rename.
#' @param new_names Character vector giving the new names for the columns
#' specified in `current_names`.
#' @template checks
#' @return Data frame specified in `df` is returned server-side with variables
#' renamed.
#' @importFrom dsBaseClient ds.assign ds.dataFrame
#' @importFrom purrr map pmap
#' @importFrom DSI datashield.connections_find
#' @keywords internal
#' @export
dh.renameVars <- function(df = NULL, current_names = NULL, new_names,
                          conns = NULL, checks = TRUE) {
  lifecycle::deprecate_warn("1.6.0", "dh.renameVars()", "dsTidyverseClient::ds.rename()")
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
    .isDefined(df = df, vars = current_names, conns = conns)
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
    datasources = conns,
    stringsAsFactors = F
  )

  dh.dropCols(
    df = df,
    vars = current_names,
    type = "remove",
    new_obj = df,
    conns = conns,
    checks = FALSE
  )

  dh.tidyEnv(
    obj = new_names,
    conns = conns,
    type = "remove"
  ) %>%
    invisible()
}

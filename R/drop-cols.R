#' Removes columns from a serverside data frame
#'
#' Often we want to remove variables from a dataframe. This function
#' allows you to specify the variables you either want to remove or keep and
#' and creates a new dataframe with only the required variables.
#'
#' @template conns
#' @template df
#' @param vars Character vector specifying columns within `df` to be removed or
#' kept.
#' @param new_obj Optionally, character specifying name for new server-side
#' data frame. Default is to return original data frame with columns removed.
#' @param type Character specifying how to treat `vars`. If "remove" these
#' variables are removed from the data frame, if "keep" these variables are
#' kept in the data frame and all others are removed.
#' @template checks
#' @param new_df_name Retired argument name. Please use `new_obj' instead.
#'
#' @return Server-side data frame the specified subset of columns.
#'
#' @family data manipulation functions
#'
#' @importFrom dsBaseClient ds.asNumeric ds.colnames ds.dataFrameSubset ds.make
#' @importFrom purrr imap map
#' @importFrom dplyr %>%
#' @importFrom DSI datashield.connections_find
#' @importFrom stringr str_subset
#'
#' @export
dh.dropCols <- function(df = NULL, vars = NULL, new_obj = df, type = NULL,
                        conns = NULL, checks = TRUE, new_df_name = NULL) {
  . <- NULL

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (!missing(new_df_name)) {
    warning("Please use `new_obj` instead of `new_df_name`")
    new_obj <- new_df_name
  }

  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  }

  type <- match.arg(type, c("remove", "keep"))

  if (type == "keep") {
    ds.dataFrame(
      x = paste0(df, "$", vars),
      newobj = new_obj,
      datasources = conns
    )
  } else if (type == "remove") {
    cols <- datashield.aggregate(conns, call("colnamesDS", df))

    cols_to_keep <- cols %>%
      map(function(x) {
        x[which(!x %in% vars)] %>%
          paste0(df, "$", .)
      })
    
    cols_to_keep %>%
      imap(
        ~ ds.dataFrame(
          x = .x,
          datasources = conns[.y],
          newobj = new_obj
        )
      )
  }
}

#' Removes columns from a serverside data frame
#'
#' Often we was to remove variables from a dataframe that we no longer need. 
#' This function allows you to specify the variables you either want to
#' keep or drop, and creates a new dataframe with only the required variables.
#'
#' @param conns DataSHIELD connections object.
#' @param df A character giving the name of a server-side data frame.
#' @param vars A character vector of columns within `df` to be removed or kept.
#' @param new_obj name for the new dataframe
#' @param type whether to remove or keep specified variables
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#' @param new_df_name Retired argument name. Please use `new_obj' instead.
#'
#' @return a new dataframe is created containing the specified subset of columns
#'
#' @importFrom dsBaseClient ds.asNumeric ds.colnames ds.dataFrameSubset ds.make
#' @importFrom purrr imap map
#' @importFrom dplyr %>%
#' @importFrom DSI datashield.connections_find
#' @importFrom stringr str_subset
#'
#' @export
dh.dropCols <- function(df = NULL, vars = NULL, new_obj = df, type = c("remove", "keep"), conns = NULL, checks = TRUE, new_df_name = NULL) {
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

  type <- match.arg(type)

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
        str_subset(x, paste0(vars, "\\b", collapse = "|"), negate = TRUE) %>%
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

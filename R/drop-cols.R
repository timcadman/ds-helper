#' Subsets a dataframe using column names.
#'
#' Often we may have created variables within a dataframe that we no longer
#' need. This function allows you to specify either the variables you want to
#' keep or drop and create a new dataframe with only the variables you need.
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#' @param vars variables to keep or remove
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

  if(checks == TRUE){
  .isDefined(obj = df, datasources = conns)
  dh.doVarsExist(df = df, vars = vars, conns = conns)
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

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

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

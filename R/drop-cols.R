#' Subsets a dataframe using column names.
#'
#' Often we may have created variables within a dataframe that we no longer
#' need. This function allows you to specify either the variables you want to
#' keep or drop and create a new dataframe with only the variables you need.
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#' @param vars variables to keep or remove
#' @param new_df_name name for the new dataframe
#' @param type whether to remove or keep specified variables
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
dh.dropCols <- function(df = NULL, vars = NULL, new_df_name = df, type = c("remove", "keep"), conns = NULL) {
  . <- NULL

  dh.doVarsExist(df = df, vars = vars, conns = conns)

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s) to remove")
  }

  type <- match.arg(type)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (type == "keep") {
    ds.dataFrame(
      x = paste0(df, "$", vars),
      newobj = new_df_name,
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
          newobj = new_df_name
        )
      )
  }
}

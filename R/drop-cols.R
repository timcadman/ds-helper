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
#' @param comp_var name of a variable with df which is complete for all subjects
#'                 (e.g. an id variable)
#' @param type whether to remove or keep specified variables
#' @param remove_temp remove temporary objects from the DataSHIELD backends
#'
#' @return a new dataframe is created containing the specified subset of columns
#'
#' @importFrom dsBaseClient ds.asNumeric ds.colnames ds.dataFrameSubset ds.make
#' @importFrom purrr imap map
#' @importFrom dplyr %>%
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.dropCols <- function(df = NULL, vars = NULL, new_df_name = NULL, comp_var = NULL, type = c("remove", "keep"), remove_temp = FALSE, conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s) to remove")
  }

  if (is.null(new_df_name)) {
    stop("Please specify name for new dataframe")
  }

  if (is.null(comp_var)) {
    stop("Please specify a reference variable, usually a unique identifier")
  }

  type <- match.arg(type)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  vars_index <- dh.findVarsIndex(df = df, vars = vars, conns = conns)

  if (type == "keep") {
    keep_vars <- vars_index
  } else if (type == "remove") {
    cols <- ds.colnames(df, datasources = conns) %>% map(~ seq(1:length(.)))

    keep_vars <- seq(1:length(names(cols))) %>%
      map(
        ~ cols[[.]][cols[[.]] %in% vars_index[[.]] == FALSE]
      )

    names(keep_vars) <- names(cols)
  }

  ds.asNumeric(paste0(df, "$", comp_var), "comp_var_num",
    datasources = conns
  )

  ds.make(
    paste0("comp_var_num", "-", "comp_var_num", "+1"), "tmp_sub_var",
    datasources = conns
  )

  keep_vars %>%
    imap(
      ~ ds.dataFrameSubset(
        df.name = df,
        V1.name = "tmp_sub_var",
        V2.name = "1",
        Boolean.operator = "==",
        keep.cols = .x,
        keep.NAs = TRUE,
        newobj = new_df_name,
        datasources = conns[.y]
      )
    )

  dh.tidyEnv(conns, obj = c("comp_var_num", "tmp_sub_var"), type = "remove")
}

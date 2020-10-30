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
#' @param cohorts optional argument specifying which cohorts to use
#' @param remove_temp remove temporary objects from the DataSHIELD backends
#'
#' @return a new dataframe is created containing the specified subset of columns
#'
#' @importFrom dsBaseClient ds.asNumeric ds.colnames ds.dataFrameSubset ds.make
#' @importFrom purrr imap map
#' @importFrom dplyr %>%
#'
#' @export
dh.dropCols <- function(conns = opals, df, vars, new_df_name, comp_var, type = c("keep", "remove"),
                        cohorts, remove_temp = FALSE) {
  if (missing(cohorts)) {
    cohorts <- names(conns)
  }

  type <- match.arg(type)

  vars_index <- dh.findVarsIndex(conns, df, vars, cohorts)

  if (type == "keep") {
    keep_vars <- vars_index
  } else if (type == "remove") {
    cols <- ds.colnames(df, datasources = conns[cohorts]) %>% map(~ seq(1:length(.)))

    keep_vars <- seq(1:length(names(cols))) %>%
      map(
        ~ cols[[.]][cols[[.]] %in% vars_index[[.]] == FALSE]
      )

    names(keep_vars) <- names(cols)
  }

  ds.asNumeric(paste0(df, "$", comp_var), "comp_var_num",
    datasources = conns[cohorts]
  )

  ds.make(
    paste0("comp_var_num", "-", "comp_var_num", "+1"), "tmp_sub_var",
    datasources = conns[cohorts]
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

  if (remove_temp == TRUE) {
    dh.tidyEnv(conns, obj = c("comp_var_num", "tmp_sub_var"), type = "remove", cohorts = cohorts)
  }
}

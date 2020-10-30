#' Creates a variable summarising how many observations each subject has in a
#' dataframe
#'
#' When conducting multilevel modelling we want to subset the data to only
#' include subjects with at least two observations. This function creates a
#' variable summarising this. Note that this is calculated based on the entire
#' dataframe, so the number provided may be greater than for the specific
#' variable you want to model. It is recommended therefore that you first
#' subset your dataframe to contain only the repeated measures variable that
#' you plan to model.
#'
#' @param conns connection objects to DataSHIELD backends
#' @param df opal dataframe
#' @param idvar ID variable in that dataset that you want to count
#'
#' @return The original dataframe with an additional variable "n_repeated_obs"
#'
#' @importFrom dsBaseClient ds.asFactor ds.asInteger ds.dataFrame ds.table ds.cbind
#'             ds.rep ds.dataFrameSort ds.asCharacter ds.tapply.assign ds.dim
#' @importFrom tidyr tibble
#'
#' @export
dh.countRM <- function(conns = opals, df = "cbcl_comp", idvar = "child_id") {
  dh.doesDfExist(conns, df)
  dh.doVarsExist(conns, df, idvar)

  id_full <- paste0(df, "$", idvar)

  ## First we need to make factor and integer versions of the id variable
  ds.asFactor(id_full, "id_fac", datasources = conns)
  ds.asInteger(id_full, "id_int", datasources = conns)
  ds.asCharacter(id_full, "id_chr", datasources = conns)

  ds.dataFrame(
    x = c(df, "id_fac", "id_int"),
    newobj = df,
    stringsAsFactors = FALSE,
    datasources = conns
  )


  ds.tapply.assign(
    X.name = "cbcl_sub$ext_raw_",
    INDEX.names = "id_chr",
    FUN.name = "sum",
    newobj = "test",
    datasources = conns
  )

  ds.summary("cbcl_sub", datasources = conns)

  ds.dim("test[[1]]", datasources = conns)

  ## Now make a table counting number of obs per id
  ds.table(
    rvar = paste0(df, "$id_fac"),
    table.assign = TRUE,
    newobj = "id_table",
    datasources = conns
  )

  # It will say it's failed, but it's lying. It's made the table, it just won't
  # show it.

  ## Now make two vectors: (i) ids and (ii) counts repeated so that we recreate
  ## the original length of the id vector and another vector the same length
  ## containing the total count for that id.

  ds.rep(
    x1 = "id_table$dimnames[[1]]",
    times = "id_table$counts",
    source.x1 = "s",
    source.times = "s",
    newobj = "tab_id_vec",
    datasources = conns
  )

  ds.rep(
    x1 = "id_table$counts",
    times = "id_table$counts",
    source.x1 = "s",
    source.times = "s",
    newobj = "tab_counts_vec",
    datasources = conns
  )

  ## Now convert the names vector from character to integer. We need to do this
  ## so that we can sort by it.
  ds.asInteger("tab_id_vec", "tab_id_int", datasources = conns)

  ds.cbind(
    x = c("tab_id_int", "tab_counts_vec"),
    newobj = "counts_df",
    datasources = conns
  )

  ## Now sort this new dataframe and the original dataframe by id. This is because
  ## we need to cbind them together and want to make sure that they are in the
  ## correct order.
  ds.dataFrameSort(
    df.name = "counts_df",
    sort.key.name = "counts_df$tab_id_int",
    sort.method = "numeric",
    newobj = "counts_sort",
    datasources = conns
  )

  ds.dataFrameSort(
    df.name = df,
    sort.key.name = paste0(df, "$id_int"),
    sort.method = "numeric",
    newobj = "cbcl_sort",
    datasources = conns
  )

  ## Join back with the original dataframe
  ds.cbind(
    x = c(df, "counts_sort"),
    newobj = df,
    datasources = conns
  )

  ## Tidy up

  # Rename created variable
  dh.renameVars(
    conns = conns,
    df = df,
    names = tibble(oldvar = "tab_counts_vec", newvar = "n_repeated_obs")
  )

  # Remove temporary columns
  dh.dropCols(
    conns = conns,
    df = df,
    vars = c("id_fac", "id_int", "tab_id_int", "tab_counts_vec"),
    comp_var = idvar,
    new_df_name = df,
    type = "remove"
  )

  # Remove temporary objects
  dh.tidyEnv(
    conns = conns,
    obj = c(
      "id_fac", "id_int", "id_table", "tab_counts_vec", "tab_id_int",
      "tab_id_vec", "cbcl_sort", "counts_df", "counts_sort"
    )
  )

  cat("Variable 'n_repeated_obs' created in supplied data frame")
}

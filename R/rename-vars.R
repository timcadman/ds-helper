#' Rename multiple variables at once
#'
#' This function allows you to rename multiple variable from a dataframe. At the
#' moment it doesn't "rename" as such, it creates duplicate variables with the
#' new names. I've left it like this to keep in the spirit of ds/opal set up
#' by not automating the deletion of variables.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df dataframe
#' @param current_names a vector with names of existing DataSHIELD variables to 
#'        rename
#' @param new_names a vector corresponding to the vector provided to current_names
#'        with the new variable names.
#' @return None. The new variables are added to the df specified 
#'
#' @importFrom dsBaseClient ds.assign ds.dataFrame
#' @importFrom purrr map pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.renameVars <- function(df = NULL, current_names = NULL, new_names, 
  conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(current_names)) {
    stop("Please specify a vector containing existing variable names")
  }

  if (is.null(new_names)) {
    stop("Please specify a vector containing the new variable names")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

names <- NULL

  dh.doesDfExist(df = df, conns = conns)
  dh.doVarsExist(df = df, vars = names$oldvar, conns = conns)

  if(length(current_names) != length(new_names)){

  stop("Length of current_names is different from the length of new_names.
    Please check input vectors")
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
    conns = conns
    )

  dh.tidyEnv(
    obj = new_names,
    conns = conns) %>%
  invisible()
}
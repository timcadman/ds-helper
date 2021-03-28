#' Rename multiple variables at once
#'
#' This function allows you to rename multiple variable from a dataframe. At the
#' moment it doesn't "rename" as such, it creates duplicate variables with the
#' new names. I've left it like this to keep in the spirit of ds/opal set up
#' by not automating the deletion of variables.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df dataframe
#' @param names a dataframe or tibble containing two columns: "oldvar" (existing
#'              variable name), "newvar" (new variable name). Each row
#'              corresponds to one variable you want to rename
#'
#' @return None. The new variables are added to the df specified
#'
#' @importFrom dsBaseClient ds.assign ds.dataFrame
#' @importFrom purrr map pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.renameVars <- function(df = NULL, names = NULL, conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(names)) {
    stop("Please specify a reference tibble containing old and new names")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }


  old_new <- newvar <- NULL

  dh.doesDfExist(conns, df)
  dh.doVarsExist(conns, df, names$oldvar)

  names %>%
    pmap(function(oldvar, newvar, ...) {
      ds.assign(
        toAssign = paste0(df, "$", oldvar),
        newobj = newvar,
        datasources = conns
      )
    })

  names(conns) %>%
    map(
      ~ ds.dataFrame(
        x = c(df, names$newvar),
        newobj = df,
        datasources = conns[.]
      )
    )

  names %>%
    pull(newvar) %>%
    map(function(x) {
      dh.tidyEnv(conns = conns, obj = x)
    }) %>%
    invisible()
}

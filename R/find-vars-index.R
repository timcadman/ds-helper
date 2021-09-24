#' Return return indices of columns in opal dataframe
#'
#' Some ds functions ask you to provide the index of variables as arguments.
#' This is highly susceptable to breaking! This is a simple function that will
#' return the index of variables for a given cohort. It is usefully used in
#' combination the "keep.cols" argument of ds.dataFrameSubset.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df datashield dataframe
#' @param vars vector of variable names in dataframe
#'
#' @return list of indices where length of list is number of cohorts provided
#'
#' @importFrom dsBaseClient ds.colnames
#' @importFrom purrr pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.findVarsIndex <- function(df = NULL, vars = NULL, conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s) to identify")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }


  dh.doVarsExist(df = df, vars = vars, conns = conns)
  dh.doesDfExist(df = df, conns = conns)

  ref_tab <- tibble(
    var = rep(vars, length(names(conns))),
    cohort = rep(names(conns), each = length(vars))
  )

  tmp <- ref_tab %>%
    pmap(
      function(var, cohort) {
        which(
          ds.colnames(df, datasources = conns[cohort])[[1]] %in% var == TRUE
        )
      }
    )

  out <- split(unlist(tmp), ceiling(seq_along(unlist(tmp)) / length(vars)))
  names(out) <- names(conns)

  return(out)
}

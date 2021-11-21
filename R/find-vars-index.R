#' Return return indices of columns in serverside dataframe from variable names
#'
#' Some DataSHIELD functions reuire column indices as parameters. This is highly 
#' susceptable to breaking as changes in code will change the order of 
#' variables. This function allows you to specify the names of columns and
#' returns their indices. 
#'
#' @param conns DataSHIELD connections object.
#' @param df A character giving the name of a server-side data frame.
#' @param vars vector of variable names in dataframe
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#'
#' @return list of indices where length of list is number of cohorts provided
#'
#' @importFrom dsBaseClient ds.colnames
#' @importFrom purrr pmap
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.findVarsIndex <- function(df = NULL, vars = NULL, conns = NULL, checks = TRUE) {
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  ## -- Make reference table of vars and cohorts -------------------------------
  ref_tab <- tibble(
    var = rep(vars, length(names(conns))),
    cohort = rep(names(conns), each = length(vars))
  )

  ## ---- Get column names for each cohort -------------------------------------
  cols <- datashield.aggregate(conns, call("colnamesDS", df))

  tmp <- ref_tab %>%
    pmap(function(var, cohort) {
      which(cols[[1]] %in% var == TRUE)
    })

  out <- split(unlist(tmp), ceiling(seq_along(unlist(tmp)) / length(vars)))
  names(out) <- names(conns)

  return(out)
}

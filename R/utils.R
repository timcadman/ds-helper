#' Internal function  to check whether object exists.
#'
#' This is a wrapper around isDefined with more flexibility.
#' It always checks if the object exist, and optionally checks if variables
#' exist within that object.
#'
#' @param df dataframe on armadillo/opal server
#' @param vars optional variables within that object
#' @param conns datashield connections object
#'
#' @importFrom utils getFromNamespace
#'
#' @noRd
.isDefined <- function(df = NULL, vars = NULL, conns = NULL) {
  extract <- utils::getFromNamespace("extract", "dsBaseClient")
  isDefined <- utils::getFromNamespace("isDefined", "dsBaseClient")

  isDefined(obj = df, datasources = conns)

  if (!is.null(vars)) {
    paste0(df, "$", vars) %>%
      map(~ isDefined(obj = df, datasources = conns))
  }
}

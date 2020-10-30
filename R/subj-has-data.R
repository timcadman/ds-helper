#' Indicate whether subject has any non-missing values in a set of variables
#'
#' At some point in the analysis you will want to subset your dataset to contain
#' only subjects meeting some criteria, e.g. data on at least one exposure and
#' one outcome. This function speeds things up by indicating whether a subject
#' has any non-missing values for a given set of variables.
#'
#' @param conns connection object for DataSHIELD backends
#' @param df datashield dataframe
#' @param vars = vector of variable names in dataframe
#' @param new_label = label which forms the suffix for the two created variables
#'
#' @return None. Two new variables are variables created within the opal
#'         environment. The first indicates how many of the variables each
#'         subject has a non-missing value on. The second indicates whether
#'         subjects have non-missing values on at least one of these variables.
#'
#' @importFrom dsBaseClient ds.Boole ds.make ds.asNumeric
#'
#' @export
dh.subjHasData <- function(conns = opals, df, vars, new_label) {
  if (missing(cohorts)) {
    cohorts <- names(conns)
  }

  dh.doesDfExist(conns = conns, df = df)

  ## ---- Convert to numeric -----------------------------------------------------

  # We do this because we will need to use ds.Boole to compare them to 0. These
  # are just numeric copies of the variables we will end up keeping.

  sapply(vars, function(x) {
    ds.asNumeric(paste0(df, "$", x), newobj = paste0(x, "_num"), datasources = conns)
  })


  ## ---- Create a vector of these numeric variable names ------------------------
  vars_num <- paste0(vars, "_num")


  ## ---- Now evaluate whether there are values >= 0 -----------------------------

  # If a value is NA it will return NA.
  sapply(vars_num, function(x) {
    ds.Boole(
      V1 = x,
      V2 = "0",
      Boolean.operator = ">=",
      na.assign = 0,
      newobj = paste0(x, "_yn"),
      datasources = conns
    )
  })


  ## ---- Count number of non-missing variables for each subject -----------------
  ds.make(
    toAssign = paste0(paste0(vars_num, "_yn"), collapse = "+"),
    newobj = paste0("n_", new_label),
    datasources = conns
  )


  ## ---- Create final variable indicating if there are any non-missing values ---
  ds.Boole(
    V1 = paste0("n_", new_label),
    V2 = "0",
    Boolean.operator = ">",
    na.assign = 0,
    newobj = paste0("any_", new_label),
    datasources = conns
  )
}

#' Indicates whether each subject has any or all of the variables contained
#' within a set
#'
#' At some point in the analysis you will want to subset your dataset to contain
#' only subjects meeting some criteria, e.g. data on at least one outcome or
#' data on all exposures. This function speeds things up by indicating whether a subject
#' has non-missing values for any or all of a set of given variables.
#' 
#' Note this function replaces the deprecated dh.subjHasData.
#' #'
#' @param conns connection object for DataSHIELD backends
#' @param df datashield dataframe
#' @param vars = vector of variable names in dataframe
#'
#' @return None. A new variable is created within the opal environment. If the option
#'         "any" is selected for argument "type", the new variable is called "dc_any_data".
#'         If the option "all" is selected, the new variable is called "dc_all_data"
#'
#' @importFrom dsBaseClient ds.Boole ds.make ds.asNumeric ds.replaceNA
#' @importFrom DSI datashield.connections_find
#' @importFrom purrr map
#' @importFrom dplyr %>%
#'
#' @export
dh.defineCases <- function(df = NULL, vars = NULL, type = c("any", "all"), conns = NULL) {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s)")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doesDfExist(conns = conns, df = df)

  ## ---- Convert to numeric -----------------------------------------------------
  vars %>%
  map(
    ~ds.asNumeric(
      x.name = paste0(df, "$", .),
      newobj = ., 
      datasources = conns)
    )

  ## Does subject have non-missing data for all of these vars?
if(type == "all"){

  ds.make(
    toAssign = paste0(vars, collapse = "+"),
    newobj = "dc_all_data", 
    datasources = conns
    ) 

  ds.replaceNA(
    x = "dc_all_data", 
    forNA = -999999, 
    newobj = "dc_all_data", 
    datasources = conns)

  ds.Boole(
    V1 = "dc_all_data", 
    V2 = -999999, 
    Boolean.operator = ">", 
    na.assign = 0, 
    newobj = "dc_all_data", 
    datasources = conns
    )

  ## Does subject have non-missing data for any of these vars?
} else if(type == "any"){

  vars %>%
  map(
    ~ds.replaceNA(
      x = ., 
      forNA = -999999, 
      newobj = .)
    ) ## Replace all NAs. All variables will now either be the original value or -999999

  vars %>%
  map(
    ~ds.Boole(
      V1 = ., 
      V2 = -999999, 
      Boolean.operator = ">", 
      newobj = paste0(., "_dc_1"))
    )

  ds.make(
    toAssign = paste0(
      paste0(vars, "_dc_1"), collapse = "+"), 
    newobj = "dc_any_data")

  ds.Boole(
    V1 = "dc_any_data", 
    V2 = 1, 
    Boolean.operator = ">=", 
    na.assign = 0,
    newobj = "dc_any_data"
    )

}

if(type == "all"){
  
  toremove <- vars

} else if(type == "any"){

  toremove <- c(vars, paste0(vars, "_dc_1"))
}

  dh.tidyEnv(
    obj = toremove, 
    type = "remove", 
    conns = conns)

}
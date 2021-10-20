#' Indicates whether each subject has any or all of the variables contained
#' within a set
#'
#' At some point in the analysis you will want to subset your dataset to contain
#' only subjects meeting some criteria, e.g. data on at least one outcome or
#' data on all exposures. This function speeds things up by indicating whether a subject
#' has non-missing values for any or all of a set of given variables.
#'
#' Note this function replaces the deprecated dh.subjHasData.
#'
#' @param conns connection object for DataSHIELD backends
#' @param df datashield dataframe
#' @param vars vector of variable names in dataframe
#' @param type whther to define cases based on any or all provided variables
#' @param newobj optional name for outputted object. Defaults to "dc_data_avail"
#' @return None. A new variable is created within the opal environment. If the option
#'         "any" is selected for argument "type", the new variable is called "dc_any_data".
#'         If the option "all" is selected, the new variable is called "dc_all_data"
#'
#' @importFrom dsBaseClient ds.Boole ds.make ds.asNumeric ds.replaceNA
#' @importFrom DSI datashield.connections_find
#' @importFrom purrr map
#' @importFrom dplyr %>%
#' @importFrom rlang arg_match
#'
#' @export
dh.defineCases <- function(df = NULL, vars = NULL, type = NULL, conns = NULL,
                           newobj = "dc_data_avail") {
  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s)")
  }
  
  if (is.null(type)) {
    stop("Please specify whether you want to define cases based on any or all of
         provided variable(s)")
  }
  
  type <- match.arg(type, c("any", "all"))

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doesDfExist(conns = conns, df = df)

  ## ---- Convert to numeric -----------------------------------------------------
  vars %>%
    map(function(x){
      calltext <- call("asNumericDS", paste0(df, "$", x))
      DSI::datashield.assign(conns, x, calltext)
      })
   
  ## Does subject have non-missing data for all of these vars?
  if (type == "all") {
    
    DSI::datashield.assign(
      conns, 
      "dc_all_data", 
      as.symbol(paste0(vars, collapse = "+")))
    
    ds.replaceNA(
      x = "dc_all_data",
      forNA = rep(-999999, length(conns)),
      newobj = "dc_all_data",
      datasources = conns
    )
    
    calltext <- call("BooleDS", "dc_all_data", -999999, 5, 0, TRUE)
    DSI::datashield.assign(conns, newobj, calltext)
    
    ## Does subject have non-missing data for any of these vars?
  } else if (type == "any") {
    vars %>%
      map(
        ~ ds.replaceNA(
          x = .,
          forNA = rep(-999999, length(conns)),
          newobj = .,
          datasources = conns
        )
      ) ## Replace all NAs. All variables will now either be the original value or -999999

    vars %>%
      map(function(x){
    
    calltext <- call("BooleDS", x, -999999, 6, 0, TRUE)
    DSI::datashield.assign(conns, paste0(x, "_dc_1"), calltext)

      })
    
  }

  if (type == "all") {
    toremove <- vars
  } else if (type == "any") {
    toremove <- c(vars, paste0(vars, "_dc_1"))
  }

  dh.tidyEnv(
    obj = toremove,
    type = "remove",
    conns = conns
  )
}

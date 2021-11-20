#' Generate a local proxy dataframe to enable local autocompletion in RStudio
#'
#' If you have a remote dataframe on the servers, it can be challenging to remember variable narmes when writing code.
#' Because the dataframes are on remote servers, the autocomplete won't work in RStudio. The purpose of this function
#' is to create a local object that has the same structure as the remote data frame and enables the use of autocomplete
#' when writing DataSHIELD code.
#'
#' For example, if you have a dataframe "D" with columns LAB_TSC, DIS_CVA and DIS_DIAB, normally you would need
#' to write:
#'
#' `ds.summary("D$LAB_TSC")` with the variable name spelled correctly (no autocomplete!) and remember quotation marks
#'
#' after running this function you can type
#'
#' `ds.summary(D$` and hit tab, and the list of variables will be displayed. Choose one and hit enter to finish up with:
#'
#' `ds.summary(D$LAB_TSC)` - no quotation marks needed either!
#'
#' @param conns connections object to DataSHIELD backends
#' @param df datashield dataframe
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#'
#' @return Creates a local proxy dataframe. Stops function if df doesn't exist or is inconsistent in one
#' of more cohorts.
#'
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.localProxy <- function(df = NULL, conns = NULL, checks = TRUE) {
  if (is.null(df)) {
      stop("`df` must not be NULL.", call. = FALSE)
  }

  if(checks == TRUE){
 .isDefined(obj = df, datasources = conns)
}

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  discrep <- dh.classDiscrepancy(conns = conns, df = df, checks = FALSE)
  if (any(discrep$discrepancy == "yes")) {
    warning("All columns not found in all cohorts, please see tibble returned and correct this", call. = FALSE)
    return(discrep)
  }
  tempDF <- data.frame(matrix(paste0("D$", discrep$variable), ncol = length(discrep$variable), nrow = 1))
  colnames(tempDF) <- discrep$variable
  assign(df, tempDF, envir = parent.frame())
}

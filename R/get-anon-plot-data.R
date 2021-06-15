#' Extracts an anonymised version of the dataset which can be used
#' to create bespoke plots (e.g. using ggplot). This calls the ds.scatterPlot
#' function but stores the data produced this function. Note you need to make
#' sure your plot window is large enough to produce the plot.
#'
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe
#' @param conns connection object for DataSHIELD backends
#'
#' @return A list of the length of the number of variables provided containing
#'         anonymised values for each subject of each cohort provided.
#'
#' @importFrom DSI datashield.connections_find
#' @importFrom dsBaseClient ds.scatterPlot
#' @importFrom purrr map set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @export
dh.getAnonPlotData <- function(df = NULL, vars = NULL, conns = NULL) {

. <- NULL

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s) to get plot data for")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  scatter <- vars %>%
    map(
      ~ ds.scatterPlot(
        x = paste0(df, "$", .),
        y = paste0(df, "$", .),
        return.coords = TRUE,
        datasources = conns
      )
    ) %>%
    set_names(., vars)

  out <- scatter %>%
    map(function(x) {
      x[[1]] %>%
        map(~ .[, "x"]) %>%
        map(as_tibble) %>%
        bind_rows(.id = "cohort")
    })

  return(out)
}

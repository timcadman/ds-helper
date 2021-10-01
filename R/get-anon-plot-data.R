#' Extracts an anonymised version of the dataset which can be used
#' to create bespoke plots (e.g. using ggplot). 
#'
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe
#' @param conns connection object for DataSHIELD backends
#'
#' @return A list of tibbles, the length of the number of variables provided.
#'         Contains anonymised values for each subject of each cohort provided.
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
    map(function(x){
      
      call <- paste0(
        "scatterPlotDS(", 
        paste0(df, "$", x), ",", 
        paste0(df, "$", x), 
        ",method.indicator = 1, k=3, noise=0.25)")
      
      DSI::datashield.aggregate(conns, call)
    }) %>% set_names(vars)
  
  out <- scatter2 %>% 
    map_depth(2, function(x){
    x[[1]] %>% as_tibble (.id = "cohort")
  }) %>%
    map(bind_rows, .id = "cohort")
  
  return(out)
}

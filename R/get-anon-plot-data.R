#' Extracts an anonymised version of the dataset which can be used
#' to create bespoke plots (e.g. using ggplot). This calls the ds.scatterPlot
#' function but stores the data produced this function. Note you need to make
#' sure your plot window is large enough to produce the plot.
#'
#' @param df opal dataframe
#' @param v1 variable in df to get plotdata for
#' @param v2 optional second variable if you want to get scatterplot data
#' @param conns connection object for DataSHIELD backends
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#'
#' @return A list of the length of the number of variables provided containing
#'         anonymised values for each subject of each cohort provided.
#'
#' @importFrom DSI datashield.connections_find
#' @importFrom dsBaseClient ds.scatterPlot
#' @importFrom purrr map set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang quo_name
#'
#' @export
dh.getAnonPlotData <- function(df = NULL, v1 = NULL, v2 = NULL, conns = NULL, checks = TRUE) {
  . <- value <- x <- y <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(v1)) {
    stop("`v1` must not be NULL. Please specify at least one variable to get plot data for.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = v1, conns = conns)
  }

  ## ---- Where only one variable is provided ------------------------------------------------
  if (is.null(v2)) {
    call <- paste0(
      "scatterPlotDS(",
      paste0(df, "$", v1), ",",
      paste0(df, "$", v1),
      ",method.indicator = 1, k=3, noise=0.25)"
    )

    scatter <- DSI::datashield.aggregate(conns, call)

    out <- scatter %>%
      map(~ .[[1]]) %>%
      map(as_tibble) %>%
      bind_rows(.id = "cohort") %>%
      dplyr::rename(!!quo_name(v1) := value)

    ## ---- Where two variables are provided -------------------------------------------------------------------
  } else if (!is.null(v2)) {
    call <- paste0(
      "scatterPlotDS(",
      paste0(df, "$", v1), ",",
      paste0(df, "$", v2),
      ",method.indicator = 1, k=3, noise=0.25)"
    )

    scatter <- DSI::datashield.aggregate(conns, call)

    out <- scatter %>%
      map(~
      tibble(
        x = .[[1]],
        y = .[[2]]
      )) %>%
      bind_rows(.id = "cohort") %>%
      dplyr::rename(
        !!quo_name(v1) := x,
        !!quo_name(v2) := y
      )
  }

  ## ---- Now we get the pooled data -------------------------------------------------------------------

  # This is just all the data for each cohort separately added together
  comb <- out %>%
    mutate(cohort = "combined")

  out <- bind_rows(out, comb)

  return(out)
}

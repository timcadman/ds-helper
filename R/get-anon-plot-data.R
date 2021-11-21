#' Extracts an anonymised version of serverside data which can be used to create 
#' bespoke plots
#'
#' Whilst DataSHIELD has basic plotting functionality, for publications you may 
#' need more flexiblity in creating plots. This function calls server-side 
#' functions which create an anonymised copy of the data, and returns this data 
#' in a client-side object. These values can then be used to create plots (e.g
#' with ggplot2). 
#'
#' This function can return two types of anonymised data: either anonymised
#' data for a single variable, or scatter-plot data for two variables.
#'
#' @template df
#' @param var_1 Character giving the name of a column within `df` for which to 
#' extract anonymised data.
#' @param var_2 Optionally, character specifying a second column within `df`. 
#' If a column is specified then scatter plot data will be returned with `var_1`
#' as the explanatory variable and `var_2` as the outcome variable. Default is 
#' NULL which returns anonymised data only for `var_1`.
#' @template conns
#' @template checks
#
#' @family functions to assist plot-making
#'
#' @return A tibble in long format containing columns 'cohort', 'var_1' and 
#' optionally `var_2`. The values for `var1` and `var_2` are the anonymised 
#' data points.
#'
#' @importFrom DSI datashield.connections_find
#' @importFrom dsBaseClient ds.scatterPlot
#' @importFrom purrr map set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang quo_name
#'
#' @export
dh.getAnonPlotData <- function(df = NULL, var_1 = NULL, var_2 = NULL, 
  conns = NULL, checks = TRUE) {
  . <- value <- x <- y <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(var_1)) {
    stop("`var_1` must not be NULL. Please specify at least one variable to get plot data for.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = var_1, conns = conns)
  }

  ## ---- Where only one variable is provided ------------------------------------------------
  if (is.null(var_2)) {
    call <- paste0(
      "scatterPlotDS(",
      paste0(df, "$", var_1), ",",
      paste0(df, "$", var_1),
      ",method.indicator = 1, k=3, noise=0.25)"
    )

    scatter <- DSI::datashield.aggregate(conns, call)

    out <- scatter %>%
      map(~ .[[1]]) %>%
      map(as_tibble) %>%
      bind_rows(.id = "cohort") %>%
      dplyr::rename(!!quo_name(var_1) := value)

    ## ---- Where two variables are provided -------------------------------------------------------------------
  } else if (!is.null(var_2)) {
    call <- paste0(
      "scatterPlotDS(",
      paste0(df, "$", var_1), ",",
      paste0(df, "$", var_2),
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
        !!quo_name(var_1) := x,
        !!quo_name(var_2) := y
      )
  }

  ## ---- Now we get the pooled data -------------------------------------------------------------------

  # This is just all the data for each cohort separately added together
  comb <- out %>%
    mutate(cohort = "combined")

  out <- bind_rows(out, comb)

  return(out)
}

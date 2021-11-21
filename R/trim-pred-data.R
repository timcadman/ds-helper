#' Trims predicted values based on min and max values provided
#'
#' This is to be used in conjuncture with `ds.predictLmer`. The new data frame
#' used for prediction may contain age or time values not present in the
#' original data. This allows you to remove all predicted values beyond a
#' minimum and maximum age. This ensures that plots only show predict values
#' over a range where there was actually data.
#'
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows filter between
#' @importFrom rlang sym
#'
#' @param pred Tibble of predicted values returned by dh.predictLmer.
#' @param coh_names Character Vector of cohort names corresponding to cohorts
#' included in `pred`.
#' @param age Character specifying name of the age variable in `pred`.
#' @param min Vector of minimum values of age with length equal to number of
#' cohorts in `pred`.
#' @param max Vector of maximum values of age with length equal to number of
#' cohorts in `pred`.
#'
#' @return `pred` is returned as a tibble with predicted values outside of
#' `min` and `max` removed.
#
#' @family trajectory functions
#'
#' @export
dh.trimPredData <- function(pred = NULL, coh_names = NULL, age = "age",
                            min = NULL, max = NULL) {
  cohort <- NULL

  if (is.null(pred)) {
    stop("Please provide a tibble of predicted values")
  }
  if (is.null(coh_names)) {
    stop("Please provide a vector cohort names")
  }
  if (is.null(min)) {
    stop("Please provide a vector of minimum age values")
  }
  if (is.null(max)) {
    stop("Please provide a vector of maximum age values")
  }
  ref <- tibble(
    coh_ref = coh_names,
    min = min,
    max = max
  )

  out <- ref %>%
    pmap(function(coh_ref, min, max) {
      pred %>%
        dplyr::filter(cohort == coh_ref & between(!!sym(age), min, max))
    }) %>%
    bind_rows()

  return(out)
}

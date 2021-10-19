#' Trims predicted values based on min and max values provided. Useful
#' where you want to ensure that predicted values are limited at the range
#' of observed values.
#'
#' @param pred a tibble of predicted values
#' @param coh_names a vector of cohort names corresponding to the cohort names in pred
#' @param age name of the variable corresponding to age in the predicted values
#' @param min a vector of minimum values of age, length equal to number of cohorts
#' @param max a vector of maximum values of age, length equal to number of cohorts
#'
#' @return The original tibble of predicted values, trimmed by the min and max ages
#'
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows filter between
#' @export

dh.trimPredData <- function(pred = NULL, coh_names = NULL, age = "age", min = NULL, max = NULL) {
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
        dplyr::filter(cohort == coh_ref & between(age, min, max))
    }) %>%
    bind_rows()

  return(out)
}

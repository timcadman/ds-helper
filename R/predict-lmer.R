#' Gets predicted values based on a new dataframe for lmer models
#'
#' Currently there is no `predict` method for lmer models within DataSHIELD.
#' This function replicates this, by calculating predicted values for fixed
#' effects based on the model coefficients. Standard errors are returned for
#' individual cohorts but yet for pooled models.

#' @param model Model object returned by ds.lmerSLMA.
#' @param new_data Tibble or data frame containing values for variables in
#' `model` at which to predict values of the outcome. The column names in
#' `new_data` must be identical to those in `model`, and all variables included
#' in `model` must be present in `new_data`.
#' @param coh_names Please specify 'conns' instead.
#' Vector of cohort names. These must be in the order that
#' cohorts were specified in `model`.
#' @return Tibble of predicted outcome values based on values provided in
#' `new_data`.
#'
#' @importFrom dplyr pull filter select group_by group_keys all_of group_split
#' rename bind_cols bind_rows mutate
#' @importFrom tidyr pivot_wider
#' @importFrom purrr set_names map pmap_df pmap_dbl
#' @importFrom tibble tibble
#' @importFrom checkmate assert_list assert_data_frame assert_character
#' assert_set_equal
#'
#' @family trajectory functions
#'
#' @export
dh.predictLmer <- function(model = NULL, new_data = NULL, coh_names = NULL) {
  validate_input(model, new_data, coh_names)

  if ("intercept" %in% colnames(new_data) == FALSE) {
    new_data <- add_intercept_column(new_data)
  }

  coefs <- extract_coefficients(model, coh_names)
  coef_names <- get_coefficient_names(coefs)
  check_new_data_cols(new_data, coef_names)
  new_data_sub <- subset_new_data(new_data, coef_names)

  coefs_wide <- reshape_coefficients(coefs, coef_names)
  coefs_by_cohort <- split_coefficients_by_cohort(coefs_wide, coef_names)
  coefs_formatted <- format_split_coefficients(coefs_by_cohort)

  products <- calculate_products(coefs_formatted, coef_names, new_data_sub)
  predictions <- calculate_predictions(products)
  vcov_by_cohort <- extract_vcov(model, coh_names)
  se <- calculate_standard_errors(model, new_data_sub, vcov_by_cohort)
  se_combined <- set_combined_as_null(se, new_data)

  predictions_ordered <- sort_list(predictions)
  se_ordered <- sort_list(se_combined)
  check_pred_match_se(predictions_ordered, se_ordered)

  new_data_predicted_se <- join_new_data_predicted_se(new_data, predictions_ordered, se_ordered)

  out <- add_confidence_intervals(new_data_predicted_se)

  return(out)
}

#' Validate input arguments for the main function.
#'
#' This function checks if the essential input arguments (model, new_data, and coh_names)
#' are not NULL and issues a warning if the deprecated parameter newdata is provided.
#'
#' @param model Model object returned by ds.lmerSLMA.
#' @param new_data The data frame of new data.
#' @param coh_names A vector of cohort names.
#' @param newdata An alternative parameter name for new_data (deprecated).
#' @return Error if checks fail, else no return.
#' @noRd
validate_input <- function(model, new_data, coh_names) {
  error_messages <- makeAssertCollection()

  assert_list(model, add = error_messages)
  assert_data_frame(new_data, add = error_messages)
  assert_character(coh_names, add = error_messages)

  return(reportAssertions(error_messages))
}



#' Add an intercept column to the new data if not present.
#'
#' @param new_data The data frame to add an intercept column to.
#' @return new_data tibble with additional column 'intercept'.
#' @noRd
add_intercept_column <- function(new_data) {
  intercept <- NULL

  new_data <- new_data %>%
    mutate(intercept = 1) %>%
    select(intercept, everything())

  return(new_data)
}

#' Returns a tibble with coefficients from the model using dh.lmTab function.
#'
#' @param model Model object returned by ds.lmerSLMA.
#' @param coh_names A vector of cohort names.
#' @return tibble, output from dh.lmtTab.
#' @noRd
extract_coefficients <- function(model, coh_names) {
  coefs <- dh.lmTab(
    model = model,
    type = "lmer_slma",
    coh_names = coh_names,
    direction = "long",
    ci_format = "separate",
    digits = 10
  )
  return(coefs)
}

#' Returns a character vector of coefficient names excluding the intercept.
#'
#' @param coefs A data frame of coefficients.
#' @return character vector with variable names from the model.
#' @noRd
get_coefficient_names <- function(coefs) {
  variable <- NULL

  coef_names <- coefs$fixed %>%
    pull(variable) %>%
    unique()
  return(coef_names)
}

#' Check all coefficients have a corresponding column in `new_data`
#'
#' @param new_data The data frame of new data.
#' @param coef_names Character vector of coefficient names from model.
#' @return Returns error if all coefficients don't have correspondong column in
#' new_data, else no return.
#'
#' @noRd
check_new_data_cols <- function(new_data, coef_names) {
  checkmate::check_subset(coef_names, colnames(new_data))
}

#' Subset `new_data` to contain only columns corresponding to `coef_names`
#'
#' @param new_data The data frame of new data.
#' @param coef_names Character vector of coefficient names from model.
#' @return Subset of new_data containing only variables in model.
#' @noRd
subset_new_data <- function(new_data, coef_names) {
  new_data_sub <- new_data %>% dplyr::select(all_of(coef_names))

  return(new_data_sub)
}

#' Reshape coefficients into wide format.
#'
#' @param coefs Coefficients ouput from `dh.lmTab`.
#' @return coefficients reshaped into wide format.
#' @noRd
reshape_coefficients <- function(coefs, coef_names) {
  cohort <- variable <- coefficient <- value <- NULL

  coefs_wide <- coefs$fixed %>%
    dplyr::select(cohort, variable, coefficient, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    dplyr::filter(coefficient == "est") %>%
    select(-coefficient)

  return(coefs_wide)
}

#' Returns a list with coefficients grouped by cohort.
#'
#' @param coef_names Coefficients in wide format, output from `reshape_coefficients`.
#' @return list of length number of cohorts, containing tibbles of coefficients.
#' @noRd
split_coefficients_by_cohort <- function(coefs_wide, coef_names) {
  cohort <- NULL

  coefs_by_cohort <- coefs_wide %>%
    group_by(cohort) %>%
    group_split(.keep = TRUE)

  return(coefs_by_cohort)
}

#' Format split coefficients by adding cohort names and removing unneeded column.
#'
#' @param split_coefficients Coefficients split by cohort: output from `split_coefficients_by_cohort`
#' @return Input list with cohort names added as list names and cohort column removed.
#' @noRd
format_split_coefficients <- function(coefs_by_cohort) {
  . <- NULL

  formatted_coefficients <- coefs_by_cohort %>%
    set_names(map(., ~ .x$cohort[1])) %>%
    map(~ dplyr::select(., -cohort))

  return(formatted_coefficients)
}

#' Multiply coefficients by new data. Returns a list of data frames with coefficients multiplied by
#' new data.
#'
#' @param coefs_by_cohort A data frame of coefficients by cohort.
#' @param coef_names A character vector of coefficient names.
#' @param new_data The data frame of new data.
#' @return list of dataframes with new_data values multiplied by coefficients.
#' @noRd
calculate_products <- function(formatted_coefficients, coef_names, new_data_sub) {
  products <- formatted_coefficients %>%
    map(function(x) {
      new_data_sub %>%
        pmap_df(function(...) {
          out <- c(...) * x
          return(out)
        })
    })
  return(products)
}

#' Calculate predictions by summing the multiplied coefficients.
#'
#' @param coefficients_by_data A list of data frames with coefficients multiplied by new data.
#' @param new_data The data frame of new data.
#' @return list of vectors of predicted values.
#' @noRd
calculate_predictions <- function(products) {
  pred <- products %>%
    map(~ rowSums(.x))

  return(pred)
}

#' Extract vcov matrices from model object; returns list of vcov grouped by cohort.
#'
#' @param model Model object returned by ds.lmerSLMA.
#' @return List of vcov matrices, list length is number of cohorts.
#' @noRd
extract_vcov <- function(model, coh_names) {
  nstudies <- 1:model$num.valid.studies
  studies_vec <- paste0("study", nstudies)

  vcov <- studies_vec %>%
    map(~ model$output.summary[[.x]]$vcov) %>%
    set_names(coh_names)

  return(vcov)
}

#' Calculate standard errors for the predictions.
#'
#' @param model Model object returned by ds.lmerSLMA.
#' @param new_data The data frame of new data.
#' @param vcov A list of covariance matrices.
#' @return list of vectors of standard errors.
#' @noRd
calculate_standard_errors <- function(model, new_data_sub, vcov) {
  se <- vcov %>%
    map(function(x) {
      new_data_sub %>%
        pmap_dbl(function(...) {
          C <- c(...)
          std.er <- sqrt(t(C) %*% x %*% C)
          out <- std.er@x
          return(out)
        })
    })

  return(se)
}

#' Creates dataset of NA values for combined estimates.
#'
#' Currently we haven't worked out how to calculate standard errors for combined estimates to
#' this makes placeholder NAs
#'
#' @param model Model object returned by ds.lmerSLMA.
#' @param new_data The data frame of new data.
#' @param vcov A list of covariance matrices.
#' @return List of SEs with extra element added as placeholder for combined
#' estimates, length equals number of rows of new_data.
#' @noRd
set_combined_as_null <- function(se, new_data) {
  se_combined <- c(se, list(combined = rep(NA, nrow(new_data))))
}

#' Sort a list based on the names of its elements.
#'
#' This function sorts a list based on the names of its elements and returns the sorted list.
#'
#' @param list_name A list to be sorted.
#' @return sorts list elements alphabetically by name.
#' @noRd
sort_list <- function(list_name) {
  ordered_list <- list_name[order(names(list_name))]
  return(ordered_list)
}

#' Check if the names of two lists match with ordered comparison.
#'
#' This function checks if the names of two lists match and are in the same order using an ordered comparison.
#
#' @param predicted_sort The first list with names to compare.
#' @param se_sort The second list with names to compare.
#' @return if names of predicted and SE lists are the same, nothing, else error.
#' @noRd
check_pred_match_se <- function(predicted_sort, se_sort) {
  checkmate::assert_set_equal(names(predicted_sort), names(se_sort), ordered = TRUE)
}

#' Join new data with predicted values and standard errors.
#'
#' This function combines new data, predicted values, and standard errors into a single tibble.
#
#' @param new_data A data frame of new data.
#' @param predicted_sort A list of predicted values with names matching new data.
#' @param se_sort A list of standard errors with names matching new data.
#' @return tibble containing columns of new_data, predicted values and SEs.
#' @noRd
join_new_data_predicted_se <- function(new_data, predicted_sort, se_sort) {
  new_pred_se <- list(predicted_sort, se_sort) %>%
    pmap(function(.x, .y) {
      new_data %>%
        mutate(
          predicted = .x,
          se = .y
        )
    }) %>%
    bind_rows(.id = "cohort")

  return(new_pred_se)
}

#' Add confidence intervals to tibble.
#'
#' This function calculates and adds lower and upper confidence intervals to a tibble
#' containing new_data, predicted values and standard errors.
#
#' @param new_pred_se A data frame containing predicted values, standard errors, and cohort
#' information.
#' @return input dataframe with 2 columns added for lower and upper standarad errors.
#' @noRd
add_confidence_intervals <- function(new_pred_se) {
  predicted <- se <- NULL

  out <- new_pred_se %>%
    mutate(
      low_ci = predicted - 1.96 * se,
      upper_ci = predicted + 1.96 * se
    )

  return(out)
}

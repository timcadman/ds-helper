#' Loop multiple GLM models and handle errors & non-convergence
#'
#' This function allows you to fit different glm models to different cohorts,
#' for example if you want to include different covariates for different
#' cohorts.
#'
#' @importFrom dplyr %>% bind_cols mutate filter
#' @importFrom dsBaseClient ds.glmSLMA
#' @importFrom purrr map pmap
#' @importFrom stringr str_detect
#'
#' @template df
#' @param ref reference tibble, output from dh.buildModels.
#' @param vary_df Option to provide different df for different models. Default
#' is FALSE.
#' @param family Family to use in glm models. Default is "gaussian".
#' @param weights Optional, serverside object containing weights to be used in the model.
#' @template checks
#' @template conns
#' @param type Type of model to run. Either "glm_slma" for fixed effects or "lmer_slma" for mixed
#' effects.
#'
#' @return Tibble containing five columns:
#' * model = Description of model fit, taken from `model_name`.
#' * formula = Formula for this model, taken from `formulae`.
#' * cohort = Cohort for which this model was fit, taken from `cohort`.
#' * fit = Output from glm model.
#' * converged = Logical; whether or not the model converged/fit'.
#'
#' @family analysis functions
#' @md
#'
#' @export
dh.multGLM <- function(df = NULL, ref = NULL, checks = TRUE, conns = NULL,
                       vary_df = F, family = "gaussian", weights = NULL, 
                       type) {
  
  formulae <- model_names <- cohort <- converged <- error_message <- NULL
    
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  .check_mult_args(df, ref, vary_df)
  
  suppressWarnings(
    model_out <- .run_models_catch_errors(ref, df, family, weights, conns, type)
  )
  
  failed <- .identify_failed_models(model_out)
  out <- .make_output(ref, model_out, failed)
  .handle_warnings(out)
  return(out)
  
}

#' Validate Inputs for Multi-Argument Functions
#'
#' Checks the validity of inputs for multi-argument functions.
#' 
#' @param df A dataframe. Must not be `NULL` if `vary_df` is `FALSE`.
#' @param ref A reference dataframe. Must not be `NULL`.
#' @param vary_df Logical. Determines if `df` can vary. Defaults to `FALSE`.
#' @return Throws an error if the conditions are not met.
#' @noRd
.check_mult_args <- function(df, ref, vary_df) {
  if (vary_df == FALSE & is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(ref)) {
    stop("`ref` must not be NULL.", call. = FALSE)
  }
}

#' Execute Models with Error Handling
#'
#' Runs models defined by reference data and handles errors during execution.
#' 
#' @param ref A reference dataframe with modeling specifications.
#' @param df A dataframe for the modeling data.
#' @param family A family type for modeling (e.g., "gaussian", "binomial").
#' @param weights A vector of weights for the model.
#' @param conns A list of DataSHIELD connections.
#' @param type A character string specifying the model type ("glm_slma" or "lmer_slma").
#' @return A list of model results or error messages.
#' @noRd
.run_models_catch_errors <- function(ref, df, family, weights, conns, type) {
  
  ref %>%
    pmap(function(formula, cohort, ...) {
      tryCatch(
        expr = .choose_model(type, formula, df, family, weights, cohort, conns),
        error = function(error_message) {
          return(list("failed", error_message))
        }
      )
    })
}

#' Choose and Execute a Model
#'
#' Selects and runs the appropriate model based on the specified type.
#' 
#' @param type A character string specifying the model type ("glm_slma" or "lmer_slma").
#' @param formula A formula for the model.
#' @param df A dataframe for the modeling data.
#' @param family A family type for modeling (e.g., "gaussian", "binomial").
#' @param weights A vector of weights for the model.
#' @param cohort A specific cohort name from the connections list.
#' @return The result of the model fitting function.
#' @noRd
.choose_model <- function(type, formula, df, family, weights, cohort, conns) {
  
  if (type == "glm_slma") {
    ds.glmSLMA(
      formula = unlist(formula),
      dataName = df,
      family = family,
      weights = weights,
      combine.with.metafor = FALSE,
      datasources = conns[cohort]
    )
  } else if (type == "lmer_slma") {
    ds.lmerSLMA(
      formula = unlist(formula),
      dataName = df,
      weights = weights, 
      combine.with.metafor = FALSE,
      datasources = conns[cohort]
    )
  }
}

#' Identify Failed Models
#'
#' Detects models that failed to converge or threw errors.
#' 
#' @param model_out A list of model outputs.
#' @return A logical vector indicating which models failed.
#' @noRd
.identify_failed_models <- function(model_out) {
  model_out %>%
    map(~ .[[1]][[1]][[1]]) %>%
    str_detect("POTENTIALLY DISCLOSIVE|failed")
}

#' Create Model Output
#'
#' Combines the reference data with model results and convergence status.
#' 
#' @param ref A reference dataframe.
#' @param model_out A list of model outputs.
#' @param failed A logical vector indicating failed models.
#' @return A dataframe with model results and convergence status.
#' @noRd
.make_output <- function(ref, model_out, failed) {
  return(
    ref |>
      mutate(
        fit = model_out, 
        converged = !failed)
  )
}

#' Handle Warnings for Model Execution
#'
#' Displays warnings based on the convergence status of models.
#' 
#' @param out A dataframe containing model results and convergence status.
#' @importFrom cli cli_alert_warning
#' @return Prints warning messages based on model convergence.
#' @noRd
.handle_warnings <- function(out) {
  if (all(out$converged == FALSE)) {
    cli_alert_warning("All models failed. Check 'convergence' table for more details")
  }
  
  if (any(out$converged == TRUE) & any(out$converged == FALSE)) {
    cli_alert_warning("Some models threw an error message. Check 'converged' column for
      more details")
  }
}


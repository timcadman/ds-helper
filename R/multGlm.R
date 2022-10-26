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
#' @template checks
#' @template conns
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
                       vary_df = F){
  formulae <- model_names <- cohort <- converged <- NULL
  
  if (vary_df == F & is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(ref)) {
    stop("`ref` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  ## ---- Sort out df ----------------------------------------------------------
  if(vary_df == F){
    
    ref <- ref %>% mutate(df = df)
    
  }
  
  ## ---- Run the models -------------------------------------------------------
  suppressWarnings(
    models <- ref %>%
      pmap(function(formula, cohort, df, ...){
        
        tryCatch(
          {
            ds.glmSLMA(
              formula = unlist(formula), 
              dataName = df, 
              family = "binomial", 
              combine.with.metafor = TRUE,
              datasources = conns[cohort]
            )
          },
          error = function(error_message) {
            out <- list("failed", error_message)
            return(out)
          }
        )
      })
  )
  
  ## ---- Identify models which failed completely ------------------------------
  fail_tmp <- models %>%
    map(~ .[[1]][[1]][[1]]) %>% 
    str_detect("POTENTIALLY DISCLOSIVE|failed", negate = TRUE)
  
  out <- ref %>%
    mutate(
      fit = models, 
      converged = fail_tmp)
  
  problems <- out %>%
    dplyr::filter(converged == FALSE)
    
  ## ---- Summarise convergence info ---------------------------------------------
  if (all(out$converged == FALSE)) {
    warning("All models failed. Check 'convergence' table for more details")
  }
  
  if (any(out$converged == TRUE) & any(out$converged == FALSE)) {
    warning("Some models threw an error message. Check 'converged' column for
      more details")
  }
  
  return(out)
}

#' This function is part of a pipeline to build regression models in DataSHIELD
#' 
#' When working in DataSHIELD you often end up finding hundreds of slightly
#' different models. For example, you might want to include differen covariates
#' for different cohorts, or perform sensitivity analyses with slight 
#' differences to the main model. This function helps with that. I will describe
#' the flow in more detail in a separate vignette, but here it is briefly:
#' 
#' dh.anyData: identify available exposures and outcomes in your data
#' dh.buildModels: based on this, build models where data is available
#' dh.multGlm: fit multiple models and store the output
#' dh.metaSepModels: meta-analyse the coefficients and return key output in a 
#' neat table
#'
#' @param avail_exp Tibble, output from dh.anyData. Should contain all 
#' exposures you want to use. One model will be created for each exposure.
#' @param avail_cov Tibble, output from dh.anyData. Should contain all 
#' covariates to include in your models. 
#' @param outcome Character giving the name of your outcome variable.
#' 
#' @return A tibble with five columns: exposure, cohort, covariates, outcome,
#' formula.
#' 
#' Currently this just builds formula for ds.lmerSLMA, but I should probably 
#' try and combine it with the lmer loop function.
#'
#' @family model building
#'
#' @importFrom purrr set_names map
#' @importFrom dplyr %>% bind_rows rename pull left_join mutate
#'
#' @export
dh.buildModels <- function(
  exp_tib = NULL, cov_tib = NULL, outcome = NULL){
  
value <- variable <- exposure <- covariates <- formula <- cohort <- coh <- 
  covs <- out <- NULL
  
  exp_coh <- exp_tib %>%
    pmap(function(...){
      
      tmp <- c(...)
      tmp[which(tmp == TRUE)] %>%
        names()
    }) %>%
    set_names(exp_tib$variable) %>%
    map(as_tibble) %>%
    bind_rows(.id = "exposure") %>%
    dplyr::rename(cohort = value)
  
  coh <- names(cov_tib)[-1]
  
  cov_coh <- coh %>%
    map(function(x){
      cov_tib %>%
        dplyr::select(variable, x) %>%
        dplyr::filter(!!sym(x) == TRUE) %>%
        pull(variable)
    }) %>%
    set_names(coh)
  
  cov_coh_tib <- tibble(
    cohort = coh,
    covariates = cov_coh
  )
  
  if(nrow(cov_tib) > 1){
    
    out <- left_join(exp_coh, cov_coh_tib, by = "cohort") %>%
      mutate(outcome = outcome)
    
    formulas <- out %>%
      pmap(function(exposure, covariates, outcome, ...){
        
        paste0(
          paste0(outcome, "~", exposure, "+"), 
          paste(unlist(covariates), collapse = "+")
        )
    })
    
    out <- out %>% mutate(formula = formulas)
    
  } else if(nrow(cov_tib) == 1){
    
    out <- left_join(exp_coh, cov_coh_tib, by = "cohort") %>%
      mutate(
        outcome = outcome,
        formula = paste0(
          outcome, "~", exposure, "+", unlist(covariates))
      ) 
  out <- out %>%
    dplyr::select(exposure, outcome, covariates, formula, cohort)

  }

  return(out)
    
}
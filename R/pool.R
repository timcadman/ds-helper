#' Perform Rubin's pooling on a list of imputed generalized linear models.
#'
#' @param imputed_glm A list of imputed generalized linear models.
#' @param type Character specifying type of object provided in `model`. Can be
#' either "glm_ipd", "glm_slma" or "lmer_slma".
#' @param coh_names Character vector of cohorts included in `imputed_glm`. Note this
#' must be in the same order as the cohorts were included in the model.
#' @param family Specifies the family used in the analysis where type is
#' "glm_ipd" or "glm_slma". Options are "gaussian" or "binomial", with default
#' "gaussian".
#' @param exponentiate Optionally, specify whether estimates from binomial models should
#' be exponentiated, ie returned as odds ratios. This argument is ignored if
#' `type` is "gaussian".
#' @return A tibble containing Rubin's pooled estimates and confidence intervals.
#'
#' @details This function performs Rubin's pooling on a list of imputed generalized linear models. 
#' It extracts coefficients using specified parameters, tidies the coefficients, and then performs 
#' pooling.
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
dh.pool <- function(imputed_glm = NULL, type = NULL, coh_names = NULL, family = NULL, 
                    exponentiate = FALSE){
  poolCheckArgs(imputed_glm, type, coh_names, family, exponentiate)
  
  m <- length(imputed_glm)
  
  coefs <- getCoefs(imputed_glm, type, coh_names, family)
  tidied_coefs <- tidyCoefs(coefs, m)
  split_coefs <- splitCoefs(tidied_coefs, type, coh_names)
  
  out <- split_coefs %>% 
    map(~makeRubinTable(coefs = .x, m = m, exponentiate = exponentiate)) %>%
    bind_rows()
  
  return(out)
  
}

#' Check arguments for dh.pool function.
#'
#' @param imputed_glm A list of imputed generalized linear models.
#' @param type Character specifying type of object provided in `model`. Can be
#' either "glm_ipd", "glm_slma" or "lmer_slma".
#' @param coh_names Character vector of cohorts included in `imputed_glm`. Note this
#' must be in the same order as the cohorts were included in the model.
#' @param family Specifies the family used in the analysis where type is
#' "glm_ipd" or "glm_slma". Options are "gaussian" or "binomial", with default
#' "gaussian".
#' @return A collection of error messages or NULL if arguments are valid.
#'
#' @details This function checks the validity of arguments passed to the dh.pool function.
#'
#' @importFrom checkmate assert_list assert_true
#' @noRd
poolCheckArgs <- function(imputed_glm, type, coh_names, family, exponentiate){
  
  error_messages <- makeAssertCollection()
  
  checkmate::assert_list(imputed_glm, add = error_messages)
  checkmate::assert_true(length(imputed_glm) > 1, add = error_messages)
  
  if(type == "glm_ipd" & length(coh_names) >1){
    warning("Your input type is `glm_ipd` but you have provided >1 cohort name. Did you intend this?
    It is recommended that the regression model is performed separately on each cohort, then estimates
    pooled and (if applicable) meta-analysed")
}
  
  if (exponentiate == TRUE & family == "gaussian") {
    warning("It is not recommended to exponentiate coefficients from linear
            regression: argument is ignored")
  }
  
  return(reportAssertions(error_messages))
  
}

#' Extract coefficients from imputed generalized linear models.
#'
#' @param imputed_glm A list of imputed generalized linear models.
#' @param type The type of coefficients to extract (e.g., "fixed", "random").
#' @param coh_names Names of the imputed datasets (cohort names).
#' @param family The family of the GLM (e.g., "binomial", "poisson").
#' @return A list of data frames containing coefficients.
#'
#' @details This function extracts coefficients from each imputed generalized linear model
#' 
#' @importFrom purrr map
#' @noRd
 getCoefs <- function(imputed_glm, type, coh_names, family){
   cohort <- NULL
   
   coefs <- imputed_glm %>%
     map(
       ~dh.lmTab(
         model = .x, 
         type = type, 
         family = family,
         coh_names = coh_names, 
         direction = "wide",
         ci_format = "separate", 
         digits = 20)
     )
   
      if(type == "glm_slma"){
         coefs <- list(coefs) %>%
           map(~dplyr::filter(., cohort != "combined"))
      }
   
   return(coefs)
       
 }

#' Tidy coefficients data frames.
#'
#' @param coefs A list of data frames containing coefficients.
#' @return A tidied data frame of coefficients.
#'
#' @details This function converts the list of coefficients into a tibble, with an extra column
#' added to specify which imputed dataset the coefficients are from.
#'
#' @importFrom dplyr bind_rows select
#' @noRd
 tidyCoefs <- function(coefs, m){
   
   imputation <- cohort <- variable <- est <- se <- n_obs <- NULL
   
   tidied <- coefs %>%
     set_names(paste0("imputation_",1:m)) %>%
     bind_rows(.id = "imputation") %>%
     dplyr::select(imputation, cohort, variable, est, se, n_obs)
 }

#' Split tidied coefficients data frame.
#'
#' @param tidied_coefs A tidied data frame of coefficients.
#' @return A list of tibbles, split by cohort and variable.
#'
#' @details This function splits the tidied coefficients tibble into a list of tibbles based on 
#' cohort and variable, ie it creates one tibble per cohort and variable, where the number of rows
#' equals the number of imputed datasets.
#'
#' @noRd
 splitCoefs <- function(tidied_coefs, type, coh_names){
   
   cohort <- variable <- NULL

    if (type == "glm_ipd"){
      tidied_coefs <- tidied_coefs %>% 
       mutate(., cohort = coh_names) 
   }
   
   split_coefs <- tidied_coefs %>% 
     group_by(cohort, variable) %>%
     group_split()

   return(split_coefs)
   
 }

#' Calculate the Rubin's pooled mean.
#'
#' @param coefs A tibble containing coefficients.
#' @return The pooled mean of coefficients.
#'
#' @details This function calculates the pooled mean using Rubin's rules from a tibble of 
#' coefficients.
#'
#' @noRd
 rubinMean <- function(coefs){
   
   return(mean(coefs$est))
   
   }

#' Calculate the Rubin's within-imputation variance.
#'
#' @param coefs A tibble containing coefficients.
#' @return The within-imputation variance.
#'
#' @details This function calculates the within-imputation variance using Rubin's rules.
#'
#' @noRd
 rubinWithinVar <- function(coefs){
   
   return(mean(coefs$se^2))
   
 }

#' Calculate the Rubin's between-imputation variance.
#'
#' @param coefs A tibble containing coefficients.
#' @param means Pooled means of coefficients.
#' @param m The number of imputations.
#' @return The between-imputation variance.
#'
#' @details This function calculates the between-imputation variance using Rubin's rules.
#'
#' @noRd
 rubinBetweenVar <- function(coefs, means, m){
   
   return(sum((coefs$est - means)^2) / (m - 1))
   
 }

#' Calculate the Rubin's pooled standard error.
#'
#' @param within_var The within-imputation variance.
#' @param between_var The between-imputation variance.
#' @param m The number of imputations.
#' @return The pooled standard error.
#'
#' @details This function calculates the pooled standard error using Rubin's rules.
#'
#' @noRd
 rubinPooledSe <- function(within_var, between_var, m){
   
   return(within_var + between_var + (between_var/m))
   
 }

#' Calculate the Rubin's Z statistic.
#'
#' @param pooled_mean The pooled mean of coefficients.
#' @param pooled_se The pooled standard error.
#' @return The Z statistic.
#'
#' @details This function calculates the Z statistic based using rules.
#'
#' @examples
#' # Example usage:
#' rubinZ(1.2, 0.3)
#'
#' @noRd
 rubinZ <- function(pooled_mean, pooled_se){
   
   return(pooled_mean / pooled_se)
   
 }

#' Calculate the Rubin's p-value.
#'
#' @param z_value The Z statistic.
#' @return The p-value.
#'
#' @details This function calculates the p-value using Rubin's rules.
#' @importFrom stats pnorm
#'
#' @noRd
 rubinP <- function(z_value){
   
   return(2 * pnorm(-abs(z_value)))
   
 }

#' Create a tibble of coefficients using Rubin's rules.
#'
#' @param coefs A tibble containing coefficients.
#' @param m The number of imputations.
#' @param exponentiate Optionally, specify whether estimates from binomial models should
#' be exponentiated, ie returned as odds ratios.
#' @return A tibble with Rubin's pooled estimates, confidence intervals, and statistics.
#'
#' @details This function creates a tibble of pooled coefficients using Rubin's rules.
#'
#' @importFrom dplyr tibble
#' @importFrom stats qnorm
#' @noRd
 makeRubinTable <- function(coefs, m, exponentiate){
   pooled_mean <- within_var <- between_var <- pooled_se <- z_value <- low_ci <- upp_ci <- NULL
   rubinTable <- tibble(
     variable = coefs$variable[[1]], 
     cohort = coefs$cohort[[1]],
     n_obs = coefs$n_obs[[1]],
     pooled_mean = rubinMean(coefs),
     within_var = rubinWithinVar(coefs),
     between_var = rubinBetweenVar(coefs, pooled_mean, m),
     pooled_se = rubinPooledSe(within_var, between_var, m),
     z_value = rubinZ(pooled_mean, pooled_se), 
     p_value = rubinP(z_value), 
     low_ci = pooled_mean - qnorm(0.975) * pooled_se, 
     upp_ci = pooled_mean + qnorm(0.975) * pooled_se)
   
   if(exponentiate){
     
     rubinTable <- rubinTable %>%
       mutate(across(c(pooled_mean, low_ci, upp_ci), ~exp(.)))

   }
   
   return(rubinTable)
   
 }
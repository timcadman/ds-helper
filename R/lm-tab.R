#' Extracts coefficients and confidence intervals from linear models
#'
#' To conveniently view model results or make tables, it is useful to extract
#' the coefficients in a useable format. This function extracts coefficients for
#' ds.glm, ds.glmSLMA and ds.lmerSLMA objects.
#'
#' @param model Object returned from either ds.glm, ds.glmSLMA or ds.lmerSLMA
#' functions.
#' @param type Character specifying type of object provided in `model`. Can be
#' either "glm_ipd", "glm_slma" or "lmer_slma".
#' @param coh_names Character vector of cohorts included in `model`. Note this
#' must be in the same order as the cohorts were included in the model.
#' @param direction Character specifying the output format. Can be either "wide"
#' or "long". See "Value" for more details.
#' @param ci_format format for the confidence intervals when direction is "wide".
#' If "separate", upper and lower confidence intervals are displayed as columns.
#' If "paste", confidence intervals are returned in the same column as the
#' coefficient within brackets. Option "paste" is only available if `ci_format` 
#' is "wide"
#' @template digits
#' @param family Specifies the family used in the analysis where type is
#' "glm_ipd" or "glm_slma". Options are "gaussian" or "binomial", with default
#' "gaussian".
#' @param exp Optionally, specify whether estimates from binomial models should
#' be exponentiated, ie returned as odds ratios. This argument is ignored if
#' `type` is "gaussian".
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% select case_when
#' @importFrom rlang arg_match
#'
#' @return A tibble. When `direction` is "wide" & `ci_format` is "paste", this
#' contains five columns:
#' * variable
#' * est
#' * lowci
#' * uppci
#' * pvalue
#'
#' When `direction` is "long", a tibble with three columns is returned:
#' * variable
#' * coefficient (containing values "est", "lowci", "uppci", "pvalue")
#' * value
#'
#' @md
#' @family descriptive functions
#'
#' @export
dh.lmTab <- function(model = NULL, type = NULL, coh_names = NULL,
                     direction = NULL, ci_format = NULL,
                     family = "gaussian", digits = 2,
                     exp = FALSE) {
  Estimate <- cohort <- se <- pooled.ML <- se.ML <- value <- coefficient <-
    variable <- est <- uppci <- pvalue <- . <- NULL
  
  check_args(model, type, direction, ci_format, family, coh_names)

  ci_names <- match_coef_names(family)
      
  ## ---- Coefficient names depending on model ---------------------------------

  ## ---- Extract coefficients -------------------------------------------------
  if (type == "glm_ipd") {
    out <- tibble(
      variable = dimnames(model$coefficients)[[1]],
      est = round(model$coefficients[, "Estimate"], digits),
      se = round(model$coefficients[, "Std. Error"], digits),
      lowci = round(model$coefficients[, ci_names$lowci], digits),
      uppci = round(model$coefficients[, ci_names$highci], digits),
      pvalue = round(model$coefficients[, "p-value"], digits),
      n_obs = model$nsubs
    ) %>%
      pivot_longer(
        cols = -variable,
        names_to = "coefficient",
        values_to = "value"
      )
    
  } else if (type == "glm_slma" | type == "lmer_slma") {
    nstudy <- model$num.valid.studies
    
    if(type == "glm_slma"){
    ns <- tibble(
      cohort = coh_names,
      n_obs = paste0("study", seq(1, nstudy, 1)) %>%
        map_int(function(x) {
          model$output.summary[[x]]$Nvalid
        }))
    
    } else if(type == "lmer_slma"){
      
      ns <- tibble(
        cohort = coh_names,
        n_obs = paste0("study", seq(1, nstudy, 1)) %>%
          map_int(function(x) {
            model$output.summary[[x]]$devcomp$dims[["N"]]
          }))
    }
    
    separate <- paste0("study", seq(1, nstudy, 1)) %>%
      map(function(x) {
        model$output.summary[[x]]$coefficients
      }) %>%
      set_names(coh_names) %>%
      map(~ as_tibble(x = ., rownames = "variable")) %>%
      bind_rows(.id = "cohort") %>%
      rename(est = Estimate) %>%
      rename(se = "Std. Error")
    
    if(type == "glm_slma"){
    
    separate <- separate %>%
      dplyr::select(cohort, variable, est, se, "Pr(>|z|)") %>%
      dplyr::rename(pvalue = "Pr(>|z|)")
    
    } else if(type == "lmer_slma"){
      
      separate <- separate %>%
        mutate(pvalue = NA)
      
    }
    
    separate <- separate %>%
      left_join(., ns, by = "cohort") %>%
      mutate(n_coh = 1)
    
    glm_slma <- model$SLMA.pooled.ests.matrix %>%
      as_tibble(rownames = "variable") %>%
      rename(est = pooled.ML) %>%
      rename(se = se.ML) %>%
      mutate(cohort = "combined") %>%
      dplyr::select(cohort, variable, est, se) %>%
      mutate(
        n_obs = sum(ns$n_obs), 
        n_coh = nstudy)
    
    ## Fix a problem where variables are not named correctly
    if (length(nstudy) == 1) {
      glm_slma$variable <- unique(separate$variable)
    }
    
    out <- bind_rows(separate, glm_slma) %>%
      group_by(variable, cohort) %>%
      group_split() %>%
      map(
        ~ mutate(
          .data = .,
          lowci = est - 1.96 * se,
          uppci = est + 1.96 * se
        )
      ) %>%
      map(
        ~ pivot_longer(
          data = .x,
          cols = c(est, se, lowci, uppci),
          names_to = "coefficient",
          values_to = "value"
        )
      ) %>%
      bind_rows()
  }
  
  out <- out %>%
    mutate(
      variable = ifelse(variable == "(Intercept)", "intercept", variable),
      value = round(value, digits)
    )
  
  
  ## ---- Convert to odds ratios where specified -------------------------------
  if (exp == TRUE & family == "gaussian") {
    warning("It is not recommended to exponentiate coefficients from linear
            regression: argument is ignored")
  } else if (exp == TRUE & family == "binomial" & direction == "long") {
    out <- out %>%
      mutate(value = case_when(
        coefficient == "pvalue" ~ value,
        coefficient %in% c("est", "lowci", "uppci") ~ round(exp(value), digits)
      ))
  } else if (exp == TRUE & family == "binomial" & direction == "wide") {
    out <- out %>%
      pivot_wider(
        names_from = c(coefficient),
        values_from = value
      ) %>%
      mutate(across(est:uppci, ~ round(exp(.), digits)))
  }
  
  ## ---- Put into final format ------------------------------------------------
  if (direction == "long") {
    out <- out
  } else if (direction == "wide" & ci_format == "separate") {
    out <- out %>%
      pivot_wider(
        names_from = coefficient,
        values_from = value
      )
  } else if (direction == "wide" & ci_format == "paste") {
    out <- out %>%
      pivot_wider(
        names_from = coefficient,
        values_from = value
      ) %>%
      mutate(est = paste0(est, " (", lowci, ",", uppci, ")")) %>%
      dplyr::select(cohort, variable, se, est, pvalue)
  }
  
  if (type == "lmer_slma") {
    
    ## Get random effects
    random_sd <- paste0("study", seq(1, nstudy, 1)) %>%
      map(function(x) {
        model$output.summary[[x]]$varcor
      }) %>%
      set_names(coh_names) %>%
      map_depth(2, function(x) {
        attr(x, "stddev")
      }) %>%
      map(as.data.frame) %>%
      map(as_tibble, rownames = "coefficient") %>%
      bind_rows(.id = "cohort") %>%
      pivot_longer(
        cols = c(-cohort, -coefficient),
        names_to = "cluster",
        values_to = "stddev"
      )
    
    ## Get std of residual error
    res_sd <- paste0("study", seq(1, nstudy, 1)) %>%
      map(function(x) {
        model$output.summary[[x]]$sigma
      }) %>%
      set_names(coh_names) %>%
      map(as.data.frame) %>%
      map(as_tibble) %>%
      bind_rows(.id = "cohort") %>%
      set_names(c("cohort", "res_std"))
    
    random_cor <- paste0("study", seq(1, nstudy, 1)) %>%
      map(function(x) {
        model$output.summary[[x]]$varcor
      }) %>%
      set_names(coh_names) %>%
      map_depth(2, function(x) {
        attr(x, "correlation")
      })
    
    out <- list(
      fixed = out,
      random_sd = random_sd,
      resid_sd = res_sd,
      random_cor = random_cor
    )
  }
  
  return(out)
}

#' Check for errors in input arguments. 
#' 
#' @return error message if any checks throw an error, else nothing.
#' 
#' @noRd
check_args <- function(model, type, direction, ci_format, family, coh_names){
  
  error_messages <- makeAssertCollection()
  
  assert(
    check_list(model),
    check_choice(type, c("glm_ipd", "glm_slma", "lmer_slma")), 
    check_choice(direction, c("long", "wide")), 
    check_choice(ci_format, c("paste", "separate")), 
    check_choice(family, c("gaussian", "binomial")), 
    add = error_messages, 
    combine = "and"
  )
  
  if(type %in% c("glm_slma", "lmer_slma")){
    
    assert(
      check_string(coh_names), 
      add = error_messages
    )
    
  }
  
  return(reportAssertions(error_messages))
  
}

#' Returns the name of the element in `model` which contains the confidence 
#' intervals. These are named differently depending on which family is used.
#' 
#' @return List with names of lower and upper confidence interval elements.
#' 
#' @noRd
match_coef_names <- function(family){
  
  coef_out <- list(
    lowci = "low0.95CI",
    highci = "high0.95CI")
  
  if (family == "binomial") {
    
    coef_out <- coef_out %>%
      map(~paste0(.x, ".LP"))
    
  }
  
  return(coef_out)
  
}

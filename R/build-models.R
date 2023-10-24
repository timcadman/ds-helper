#' Build Exposure-Outcome Models with Optional Covariates
#'
#' This function constructs exposure-outcome models for each cohort using
#' available exposure and outcome variables, and optionally available covariates.
#'
#' @param avail_exp Output from `dh.anyData` detailing available exposures for each cohort. 
#' @param avail_out Output from `dh.anyData` detailing available outcomes  for each cohort.
#' @param avail_cov Output from `dh.anyData` detailing available exposures for each cohort (optional).
#' @details The function performs the following steps:
#'   1. Summarizes available exposure and outcome variables.
#'   2. Combines available exposure and outcome variables for all cohorts.
#'   3. Tidies up the combinations.
#'   4. If covariates are available, it splits combinations by cohort, adds covariates, and creates 
#'   formulas.
#'   5. Returns the constructed models or formulas.
#' @return A tibble containing four columns: (i) cohort, (ii) exposure, (iii) outcome, (iv) 
#' regression formula with these combinations. Optionally contains fifth column with list of 
#' covariates if `avail_cov` is not null.
#' @importFrom dplyr %>% group_by group_split pull rename mutate bind_rows
#' @importFrom purrr pmap set_names map
#' @importFrom tibble as_tibble
#'
#' @export
dh.buildModels <- function(
    avail_exp = NULL, avail_cov = NULL, avail_out = NULL){
  
  exp_avail <- summarise_available(avail_exp)
  out_avail <- summarise_available(avail_out)
  
  all_combinations <- combine_exposure_outcome(exp_avail, out_avail)
  
  tidy_combinations <- tidy_all_combinations(all_combinations)
  
  if(is.null(avail_cov)){
    
    out <- make_exp_out_form(tidy_combinations)
    
  } else{
    
    cov_avail <- summarise_available(avail_cov) 
    tidy_combinations_split <- split_combinations_by_cohort(tidy_combinations)
    comb_with_covariates <- join_covariates(tidy_combinations_split, cov_avail)
    out <- make_exp_out_cov_form(comb_with_covariates)
    
  }
  
  return(out)
 

}

#' Summarise Available Exposure and Outcome Variables
#'
#' This subfunction converts the input tibble showing availability of variables into a list 
#' containing the available variables for each cohort.
#'
#' @param anydata_obj Output from `dh.anyData`.
#' @details The function extracts available variables and organizes them by cohort.
#' @return A list of available variables grouped by cohort.
#' @noRd
summarise_available <- function(anydata_obj){
  
  value <- cohort <- . <- NULL
  
  anydata_obj %>%
    pmap(function(...){
      
      tmp <- c(...)
      tmp[which(tmp == TRUE)] %>%
        names()
    }) %>%
    set_names(anydata_obj$variable) %>%
    map(as_tibble) %>%
    bind_rows(.id = "variable") %>%
    dplyr::rename(cohort = value) %>% 
    group_by(cohort) %>%
    group_split %>%
    set_names(map(.,~.x$cohort[1])) %>%
    map(~pull(.,variable))
  
}

#' Combine Available Exposure and Outcome Variables
#'
#' This subfunction takes the output from `summarise_available` and returns a list of dataframes
#' (length number of cohorts) with all combinations of exposures and outcomes for each cohort
#'
#' @param exp_avail A list of available exposure variables for each cohort.
#' @param out_avail A list of available outcome variables for each cohort.
#' @return A list of combinations of exposure and outcome variables.
#' @noRd
combine_exposure_outcome <- function(exp_avail, out_avail){
  
  combinations <- list(exp_avail, out_avail) %>%
    pmap(function(.x, .y){
      
      expand.grid(.x, .y)
      
    })
  
  return(combinations)
  
}

#' Tidy Up Combinations
#'
#' This subfunction tidies up the output of `combine_exposure_outcome`.
#'
#' @param all_combinations Output from `combine_exposure_outcome`
#' @return A tidier tibble
#' @noRd
tidy_all_combinations <- function(all_combinations){
  
  cohort <- Var1 <- Var2 <- NULL
  
  all_combinations %>%
    bind_rows(.id = "cohort") %>%
    as_tibble %>%
    dplyr::rename(cohort = cohort, exposure = Var1, outcome = Var2)
  
}

#' Create Exposure-Outcome Models
#'
#' This subfunction creates a character string formula using for exposure-outcome models without 
#' covariates which can serve as input into ds.glmSLMA or ds.glm.
#'
#' @param tidy_comb A tibble with tidy combinations of exposure and outcome variables.
#' @return A tibble containing three columns: (i) cohort, (ii) exposure, (iii) regression formula 
#' with all combinations of available exposure and outcome for each cohort.
#' @noRd
make_exp_out_form <- function(tidy_comb){
  
  outcome <- exposure <- NULL
  
  formula <- tidy_comb %>%
    mutate(formula = paste0(outcome, "~1+", exposure))
  
  return(formula)
  
}

#' Split Combinations by Cohort
#'
#' This subfunction splits the ouput of `tidy_all_combinations` into a list of tibbles grouped by 
#' cohort. We do this because each cohort may have different covariates, so we need to be join the cohort
#' specific covariates to the cohort-specific models.
#'
#' @param tidy_combinations Tidy combinations of exposure and outcome variables.
#' @return A list tibbles of exposure-outcome combinations split by cohort.
#' @noRd
split_combinations_by_cohort <- function(tidy_combinations){
  
  cohort <- . <- NULL
  
  combinations_split <- tidy_combinations %>%
    group_by(cohort) %>%
    group_split %>%
    set_names(map(., ~.x$cohort[1]))
  
}

#' Join Covariates with exposure-outcome tibble
#'
#' This subfunction joins covariates with exposure-outcome combinations.
#'
#' @param comb_split Output from `split_combinations_by_cohort`.
#' @param cov_avail Output from `summarise_available`, ie list showing available covariates.
#' @return A tibble containing four columns: (i) cohort, (ii) exposure, (iii) outcome, and (iv) 
#' a list of covariates. This represents all combinations of exposures and outcomes, and the same
#' list of covariates for each cohort repeated.
#' @noRd
join_covariates <- function(comb_split, cov_avail){
  
  list(comb_split, cov_avail) %>%
    pmap(function(.x, .y){
      
      .x %>%  mutate(covariates = list(paste0(.y)))
      
    }) %>%
    bind_rows(.id = "cohort")
  
}

#' Create final Exposure-Outcome Models with Covariates
#'
#' This subfunction adds formula to output tibble where covariates are present.
#'
#' @param comb_with_covariates Output from `join_covariates`.
#' @return A tibble containing five columns: (i) cohort, (ii) exposure, (iii) outcome, (iv)
#' list of covariates for each cohort, and (v) regression formula including exposure, outcome and 
#' covariates.
#' @noRd
make_exp_out_cov_form <- function(comb_with_covariates){
  
  outcome <- exposure <- covariates <- NULL
  
  formula <- comb_with_covariates %>%
    mutate(formula = paste0(outcome, "~1+", exposure, "+", paste0(unlist(covariates), collapse = "+")))
  
  return(formula)
}

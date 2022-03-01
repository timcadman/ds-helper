#' Function in progress to meta-analyse separate models.
#' 
#' @param ref Tibble, output from dh.multGlm. 
#' @param exp Logical, whether to exponentiate coefficients after meta-analysis
#' @param method Method of meta-analysing coefficients.
#' @param coh_out Logical, whether to include cohort coefficients in output 
#' along with meta-analysed results#' 
#' 
#' @return A tibble
#'
#' @family model building
#'
#' @importFrom rlang arg_match
#' @importFrom purrr pmap set_names map pmap_df map_int
#' @importFrom dplyr %>% bind_rows group_by group_keys group_split mutate select
#' left_join
#' @importFrom metafor rma.uni
#' @importFrom tibble tibble
#'
#' @export
dh.metaSepModels <- function(ref = NULL, exp = NULL, method = NULL, 
                             output = "both"){
  
  exposure <- variable <- cohort <- . <- est <- lowci <- uppci <- NULL
  
  method <- arg_match(
    arg = method, 
    values = c("DL", "HE", "HS", "HSk", "SJ", "ML", "REML", "EB", "PM", "GENQ")
  )
  
  output <- arg_match(
    arg = output, 
    values = c("meta", "cohort", "both")
  )
  
  if(output %in% c("meta", "both") == TRUE){
    ## ---- Get coefficients -----------------------------------------------------
    model_coefs <- ref %>%
      pmap(function(cohort, fit, ...){
        
        dh.lmTab(
          model = fit,
          coh_names = cohort,
          type = "glm_slma",
          ci_format = "separate",
          direction = "wide",
          family = "binomial",
          digits = 50)   %>%
          dplyr::filter(cohort != "combined") 
      }) %>%
      set_names(ref$exposure) %>%
      bind_rows(.id = "exposure")
    
    ## ---- Create tibble respecting grouping order ------------------------------
    model_holder <- model_coefs %>%
      group_by(exposure, variable) %>%
      group_keys 
    
    ## ---- Meta-analyse ---------------------------------------------------------
    ma.fit <- model_coefs %>%
      group_by(exposure, variable) %>%
      group_split() %>%
      map(
        ~rma.uni(
          yi = .x$est, 
          sei = .x$se, 
          method = method, 
          control=list(stepadj=0.5, maxiter=1000)))
    
    ## ---- Put back together ----------------------------------------------------
    model_holder <- model_holder %>%
      mutate(meta = ma.fit)
    
    ma.out <- model_holder %>%
      pmap_df(function(exposure, variable, meta){
        
        tibble(
          exposure = exposure,
          variable = variable,
          est = meta$beta[1],
          se = meta$se,
          lowci = meta$ci.lb,
          uppci = meta$ci.ub,
          i2 = round(meta$I2, 1), 
          n_studies = meta$k)
        
      }) %>%
      mutate(metafor_obj = model_holder$meta)
    
  }
  
  ## ---- Calculate sample size for each cohort & analysis ---------------------
  sample_n_coh <- ref %>%
    dplyr::select(exposure, cohort) %>%
    mutate(
      valid_n = ref$fit %>%
        map_int(function(x){
          
          x$output.summary$study1$Nvalid
        })
    )
  
    ## ---- Calculate combined sample size ---------------------------------------
    labs <- sample_n_coh %>% group_by(exposure) %>% group_keys %>% unlist
    
    sample_n_comb <- sample_n_coh %>%
      group_by(exposure) %>%
      group_split %>%
      map(function(x){
        
        tibble(valid_n = sum(x$valid_n))
        
      }) %>%
      set_names(labs) %>%
      bind_rows(.id = "exposure")
   
  if(output %in% c("cohort", "both") == TRUE){
  
    coh.out <- ref %>%
      pmap(function(model, cohort, fit, ...){
        
        dh.lmTab(
          model = fit,
          coh_names = cohort,
          type = "glm_slma",
          direction = "wide",
          family = "binomial", 
          ci_format = "separate") %>%
          dplyr::filter(cohort != "combined")
        
      }) %>%
      set_names(ref$exposure) %>%
      bind_rows(.id = "exposure")
    
  }
  
  if(output == "both"){
    
    both.out <- coh.out %>%
      mutate(
        i2 = NA, 
        n_studies = 1, 
        metafor_obj = NA, 
        weight = 1 / (se^2)) %>%
      group_by(exposure, variable) %>%
      group_split() %>%
      map(~mutate(., weight_scaled = (weight / sum(weight)) *100)) %>%
      bind_rows() %>%
      ungroup() %>% 
      left_join(., sample_n_coh, by = c("exposure", "cohort"))
    
    ma.out <- left_join(ma.out, sample_n_comb, by = "exposure") %>%
      mutate(cohort = "combined")
    
    out <- bind_rows(both.out, ma.out)
    
  } else if(output == "cohort"){
    
    out <- coh.out
    
  } else if(output == "meta"){
    
    out <- ma.out
    
  }
  
  if(exp == TRUE){
    
    out <- out %>%
      mutate(across(c(est, lowci, uppci), ~exp(.)))
    
  }
  
  return(out)
  
}
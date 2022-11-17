#' Function in progress to meta-analyse separate models.
#' 
#' @param ref Tibble, output from dh.multGlm. 
#' @param exp Logical, whether to exponentiate coefficients after meta-analysis
#' @param method Method of meta-analysing coefficients.
#' @param output Character; "cohort" to return cohort coefficients only, "meta"
#' to return meta-analysed coefficients only, "both" to return both.
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
      set_names(ref$model_id) %>%
      bind_rows(.id = "model_id")
    
    ## ---- Create tibble respecting grouping order ------------------------------
    model_holder <- model_coefs %>%
      group_by(model_id, variable) %>%
      group_keys 
    
    ## ---- Meta-analyse ---------------------------------------------------------
    ma.fit <- model_coefs %>%
      group_by(model_id, variable) %>%
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
      pmap_df(function(model_id, variable, meta){
        
        tibble(
          model_id = model_id,
          variable = variable,
          est = meta$beta[1],
          se = meta$se,
          lowci = meta$ci.lb,
          uppci = meta$ci.ub,
          i2 = round(meta$I2, 1), 
          n_coh = meta$k)
        
      }) %>%
      mutate(metafor_obj = model_holder$meta)
    
  }
  
  ## ---- Calculate combined sample size ---------------------------------------
    sample_n_comb <- model_coefs %>%
      group_by(model_id, variable) %>%
      group_split %>%
      map(function(x){
        
        x %>% 
          mutate(n_obs = sum(x$n_obs)) %>%
          dplyr::select(model_id, variable, n_obs) %>%
          slice(1)
      }) %>%
      bind_rows
   
    ma.out <- left_join(ma.out, sample_n_comb, by = c("model_id", "variable")) %>%
      mutate(cohort = "combined")
    
  if(output %in% c("cohort", "both") == TRUE){
  
    coh.out <- ref %>%
      pmap(function(model, cohort, fit, ...){
        
        dh.lmTab(
          model = fit,
          coh_names = cohort,
          type = "glm_slma",
          direction = "wide",
          family = "binomial", 
          ci_format = "separate", 
          digits = 50) %>%
          dplyr::filter(cohort != "combined")
        
      }) %>%
      set_names(ref$model_id) %>%
      bind_rows(.id = "model_id")
    
  }
  
  if(output == "both"){
    
    both.out <- coh.out %>%
      mutate(
        i2 = NA, 
        metafor_obj = NA,
        weight = 1/(se^2)) %>%
      group_by(model_id, variable) %>%
      group_split() %>%
      map(~mutate(., weight_scaled = (weight / sum(weight)*100))) %>%
      bind_rows %>%
      ungroup 

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
    
    out <- left_join(out, ref, by = c("model_id", "cohort")) 
  
  return(out)
  
}
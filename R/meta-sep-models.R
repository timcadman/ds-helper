dh.metaSepModels <- function(ref = NULL, exp = NULL, method = NULL, 
                             coh_out = TRUE){
  
  method <- arg_match(
    arg = method, 
    values = c("DL", "HE", "HS", "HSk", "SJ", "ML", "REML", "EB", "PM", "GENQ")
  )
  
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
        method = method))
  
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
  
  ## ---- Join back in ---------------------------------------------------------
  ma.out <- left_join(ma.out, sample_n_comb, by = "exposure")
  
  
  ## ---- Sort out cohort specific values --------------------------------------
  if(coh_out == TRUE){
    
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
      bind_rows(.id = "exposure") %>%
      mutate(
        i2 = NA, 
        n_studies = 1, 
        metafor_obj = NA) %>%
      left_join(., sample_n_coh, by = c("exposure", "cohort")) 
    
    ma.out <- ma.out %>%
      mutate(cohort = "combined")
    
    out <- bind_rows(coh.out, ma.out)
    
  }
  
  if(exp == TRUE){
    
    out <- out %>%
      mutate(across(c(est, lowci, uppci), ~exp(.)))
    
  }
  
  return(out)
  
}
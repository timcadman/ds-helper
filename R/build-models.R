dh.buildModels <- function(
    avail_exp = NULL, avail_cov = NULL, avail_out = NULL){
  
  exp_out_avail <- list(avail_exp, avail_out) %>%
    map(summarise_available)
  
  exp_out_for_models <- exp_out_avail %>%
    pmap(function(.x, .y){
      
      expand.grid(.x, .y)
      
    }) %>%
    bind_rows(.id = "cohort") %>%
    as_tibble %>%
    dplyr::rename(cohort = cohort, exposure = Var1, outcome = Var2)
  
  
  if(is.null(avail_cov)){
    
    out <- exp_out_for_models %>%
      mutate(formula = paste0(outcome, "~1+", exposure))
    
  } else{
    
    cov_avail <- summarise_available(avail_cov) 
    
    exp_out.list <- exp_out_for_models %>%
      group_by(cohort) %>%
      group_split %>%
      set_names(map(., ~.x$cohort[1]))
    
    exp_out_cov_for_models <- list(exp_out.list, cov_avail) %>%
      pmap(function(.x, .y){
        
        .x %>%  mutate(covariates = list(paste0(.y)))
        
      }) %>%
      bind_rows(.id = "cohort")
    
    out <- exp_out_cov_for_models %>%
      mutate(formula = paste0(outcome, "~1+", exposure, "+", paste0(unlist(covariates), collapse = "+")))
    
  }
  
  return(out)
  
}

summarise_available <- function(anydata_obj){
  
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
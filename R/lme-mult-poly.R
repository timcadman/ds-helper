#' Function to perform every combination of MLM fractional polynomials
#'
#' @param conns connection objects for DataSHIELD backends
#' @param df name of dataFrame
#' @param formulae tibble containing formulae with column labelled 'formulae'
#'
#' @importFrom dsBaseClient ds.lmerSLMA 
#' @importFrom purrr map flatten_chr map set_names
#' @importFrom dplyr arrange bind_rows dense_rank group_split mutate select
#'             starts_with
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_detect str_remove
#' @importFrom tibble tibble 
#' 
#' @author Tim Cadman
#'
#' @export
dh.lmeMultPoly <- function(conns = NULL, df, formulae) {
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  
  loglik <- model <- study <- log_rank <- . <- av_rank <- desc <- NULL

  ## ---- Run the models ---------------------------------------------------------
  models <- formulae$formulae %>%
    map(
      ~ ds.lmerSLMA(
        dataName = df,
        formula = .x,
        datasources = conns
      )
    )

  ## ---- Summarise convergence info ---------------------------------------------
  convergence <- models %>% map(~ .x$Convergence.error.message)
  names(convergence) <- formulae$polys

  if (all(str_detect(flatten_chr(convergence), "no convergence error reported") != TRUE)) {
    warning("Not all models have converged for all cohorts. Check 'convergence' table for more details")
  }

  ## ---- Summarise fit info -----------------------------------------------------
  nstudies <- paste0("study", seq(1, length(conns), 1))
  
  fit.tab <- models %>% map(function(x){
    
    nstudies %>% map(function(y){  
    
    tibble(
      loglik = x$output.summary[[y]]$logLik
    )
    })
  
})
  
  fit.tab <- fit.tab %>% map(function(x){set_names(x, nstudies)})
  names(fit.tab) <- formulae$polys
  fit.tab <- fit.tab %>% map(unlist)
  
  fit.tab <- bind_rows(fit.tab, .id = "model")
  
  fit.tab %<>% pivot_longer(
    cols = !model, 
    names_to = "study",
     values_to = "loglik"
  ) %>% group_split(study)
  

  fit.tab %<>% map(function(x){
    mutate(x, log_rank = dense_rank(desc(x$loglik))) %>%
      arrange(log_rank)
    
    })
  
  fit.tab <- bind_rows(fit.tab) %>% pivot_wider(
    names_from = study, 
    values_from = c(loglik, log_rank)) 
  
  colnames(fit.tab) <- str_remove(colnames(fit.tab), ".loglik")
  
  fit.tab %<>% mutate(av_rank = rowMeans(select(., starts_with("log_rank")), na.rm = TRUE)) %>%
    arrange(av_rank)
  
  out <- list(models = models, convergence = convergence, fit = fit.tab)

  return(out)
}

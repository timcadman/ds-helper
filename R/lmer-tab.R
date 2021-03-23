#' Extracts the coefficients and confidence intervals from a ds.lmerSLMA model.
#'
#' @param fit saved output from ds.lmerSLMA model
#' @param coh vector of cohorts to include
#' @param coh_format whether cohorts are in columns or rows
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate %>% select bind_cols rename mutate_at
#' @importFrom rlang arg_match
#'
#' @return A tibble containing the estimate with lower and upper confidence intervals
#'
#' @export
dh.lmerTab <- function(fit, coh, coh_format = "cols") {
  
  cohort <- NULL
  
  bystudy <- as_tibble(fit$betamatrix.valid, rownames = "coefficient")
  combined <- as_tibble(fit$SLMA.pooled.ests.matrix, rownames = "coefficient")

  coeftab <- bind_cols(bystudy, combined[, "pooled.ML"])
  names(coeftab) <- c("coefficient", coh, "combined")
  
  if(coh_format == "cols"){
  
    return(coeftab)
    
  } else if(coh_format == "rows"){
      
       out <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
        filter(cohort != "coefficient")
      
      colnames(out) <- c("cohort", coeftab$coefficient)
      
      out %<>% dplyr::rename("intercept" = '(Intercept)')
      
      out %<>% mutate_at(vars(-cohort), as.numeric)
      
      return(out)
    }
  }

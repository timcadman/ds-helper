dh.lmerTab <- function(fit, coh, coh_format = "cols") {
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

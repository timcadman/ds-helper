#' Extracts the coefficients and confidence intervals from a ds.lmerSLMA model.
#'
#' @param x saved output from ds.lmerSLMA model
#' @param type either "ipd" or "slma" depending on the type of analysis done
#' @param format format to use when running glm
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% select
#' @importFrom rlang arg_match
#'
#' @return A tibble containing the estimate with lower and upper confidence intervals
#'
#' @export
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

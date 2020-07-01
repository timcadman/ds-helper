################################################################################
## Project: ds-cs-functions
## Script purpose: Extract coefficients from regression models into table
## Date: 11th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# Simple function to extract key coefficients from regression models. 
#
# Arguments:
#
# model = saved output from either ds.glm or ds.glmSLMA
# type = either "ipd" or "slma" depending on the type of analysis done
#
# Value:
#
# A tibble containing the estimate with lower and upper confidence intervals

cs.regTab <- function(model, type){
  
  if(type == "slma"){
    
    out <- tibble(
      var = dimnames(model$SLMA.pooled.ests.matrix)[[1]],
      est = round(model$SLMA.pooled.ests.matrix[, "pooled.ML"], 2),
      lower = round(
        model$SLMA.pooled.ests.matrix[, "pooled.ML"] - 1.96 * 
          model$SLMA.pooled.ests.matrix[, "se.ML"], 2),
      upper = round(model$SLMA.pooled.ests.matrix[, "pooled.ML"] + 1.96 * 
                      model$SLMA.pooled.ests.matrix[, "se.ML"], 2)
    )
    
  } else if(type == "ipd"){
    
    out <- tibble(
      var = dimnames(model$coefficients)[[1]],
      est = round(model$coefficients[, "Estimate"], 2),
      lower = round(model$coefficients[, "low0.95CI"], 2),
      upper = round(model$coefficients[, "high0.95CI"], 2))
    
  }
  
  out %<>%
    mutate(
      est = paste0(out$est, " (", out$lower, ", ", out$upper, ")")) %>%
    select(-lower, -upper)
  
  return(out)
  
}
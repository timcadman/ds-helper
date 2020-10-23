#' Extracts the coefficients and confidence intervals from a ds.glm or 
#' ds.glmSLMA model.
#'
#' @param model saved output from either ds.glm or ds.glmSLMA
#' @param type either "ipd" or "slma" depending on the type of analysis done
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% select
#'
#' @return A tibble containing the estimate with lower and upper confidence intervals
#'
#' @author Tim Cadman
#'
#' @export
dh.glmTab <- function(x, type, format = "paste"){
  
  type <- rlang::arg_match(type, c("ipd", "slma"))
  format <- rlang::arg_match(format, c("paste", "separate"))
  
  if(type == "ipd"){
    
    out <- tibble(
      variable = dimnames(x$coefficients)[[1]],
      est = round(x$coefficients[, "Estimate"], 2),
      lower = round(x$coefficients[, "low0.95CI"], 2),
      upper = round(x$coefficients[, "high0.95CI"], 2))
    
  } else if(type == "slma"){
    
    out <- tibble(
      variable = dimnames(x$SLMA.pooled.ests.matrix)[[1]],
      est = round(x$SLMA.pooled.ests.matrix[, "pooled.ML"], 2),
      lower = round(
        x$SLMA.pooled.ests.matrix[, "pooled.ML"] - 1.96 * 
          x$SLMA.pooled.ests.matrix[, "se.ML"], 2),
      upper = round(x$SLMA.pooled.ests.matrix[, "pooled.ML"] + 1.96 * 
                      x$SLMA.pooled.ests.matrix[, "se.ML"], 2))
  
  }

  if(format == "paste"){
    
    out <- out %>%
      mutate(
      estimate = paste0(est, " (", lower, ", ", upper, ")")) %>%
      select(variable, estimate)
    
  } else if(format == "separate"){
    
    out <- out
    
    }
  
  return(out)
  
}
#' Wrapper to manaully perform two-stage meta-analysis using metafor
#' 
#' ds.SLMA functions automatically perform two-stage meta-analysis using the
#' metafor package. However in some circumstances you might want to use a 
#' different package or chose different options in metaphor. This allows you
#' to meta-analyse multiple coefficients in one function call.
#'
#' @param fit output from ds.glmSLMA
#' @param method method of meta-analyis which can be any valid method for the 
#'               metafor package
#'
#' @return list containing (i) metafor output and (ii) summary tibble of 
#'         statistics
#'
#' @importFrom purrr map map_dbl
#' @importFrom tidyverse tibble
#' @importFrom metafor rma
#'
#' @export
dh.metaManual <- function(fit, method = "ML"){
  
  nvar <- seq(1, nrow(fit$betamatrix.valid), 1)
  
  ma <- nvar %>%
    map(function(x){
      rma(
        yi = fit$betamatrix.valid[x, ],
        sei = fit$sematrix.valid[x, ], 
        method = "ML")
    })
  
  coefs <- tibble(
    term = dimnames(fit$betamatrix.valid)[[1]],
    coef = ma %>% map_dbl(function(x){x[["beta"]]}),
    se = ma %>% map_dbl(function(x){x[["se"]]})
  )
  
  out <- list(fit = ma, coefs =coefs)
  
  return(out)
  
}
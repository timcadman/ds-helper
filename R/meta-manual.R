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
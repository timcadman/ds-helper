#' Wrapper to manaully perform two-stage meta-analysis using `metafor`
#'
#' ds.SLMA functions automatically perform two-stage meta-analysis using the
#' metafor package. However in some circumstances you might want to chose 
#' different options in metafor than those provided within DataSHIELD. This 
#' function takes the output from ds.glmSLMA and manually performs the 
#' meta-analysis. You could also modify this function to use different 
#' meta-analysis packages.
#'
#' @param model output from ds.glmSLMA
#' @param method method of meta-analyis which can be any valid method for the
#'               metafor package
#'
#' @return list containing (i) metafor output and (ii) summary tibble of
#'         statistics
#'
#' @importFrom purrr map map_dbl
#' @importFrom tibble tibble
#' @importFrom metafor rma
#'
#' @export
dh.metaManual <- function(model = NULL, method = "ML") {
  if (is.null(model)) {
    stop("`model` must not be NULL.", call. = FALSE)
  }

  nvar <- seq(1, nrow(model$betamatrix.valid), 1)

  ma <- nvar %>%
    map(function(x) {
      rma(
        yi = model$betamatrix.valid[x, ],
        sei = model$sematrix.valid[x, ],
        method = "ML"
      )
    })

  coefs <- tibble(
    term = dimnames(model$betamatrix.valid)[[1]],
    coef = ma %>% map_dbl(function(x) {
      x[["beta"]]
    }),
    se = ma %>% map_dbl(function(x) {
      x[["se"]]
    })
  )

  out <- list(model = ma, coefs = coefs)

  return(out)
}

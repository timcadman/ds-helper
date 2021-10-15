#' Function to perform every combination of MLM fractional polynomials
#'
#' @param conns connection objects for DataSHIELD backends
#' @param df name of dataFrame
#' @param formulae a vector of model formulae to fit
#'
#' @importFrom dsBaseClient ds.lmerSLMA
#' @importFrom purrr map flatten_chr map set_names
#' @importFrom dplyr arrange bind_rows dense_rank group_split mutate select
#'             starts_with desc across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_detect str_remove
#' @importFrom tibble tibble
#' @importFrom DSI datashield.connections_find
#'
#' @author Tim Cadman
#'
#' @export
dh.lmeMultPoly <- function(df = NULL, formulae = NULL, poly_names = NULL, conns = NULL) {
  if (is.null(df)) {
    stop("Please specify dataframe to use for polynomial models")
  }

  if (is.null(formulae)) {
    stop("Please specify a vector of model formulae")
  }

  if (is.null(poly_names)) {
    stop("Please specify a vector of names for your models")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }


  loglik <- model <- study <- log_rank <- . <- av_rank <- loglik_study1 <- loglik_study2 <- NULL

  ## ---- Run the models ---------------------------------------------------------
  models <- formulae %>%
    map(
      ~ ds.lmerSLMA(
        dataName = df,
        formula = .x,
        datasources = conns
      )
    )

  ## ---- Summarise convergence info ---------------------------------------------
  convergence <- models %>% map(~ .x$Convergence.error.message)
  names(convergence) <- poly_names

  if (all(str_detect(flatten_chr(convergence), "no convergence error reported") != TRUE)) {
    warning("Not all models have converged for all cohorts. Check 'convergence' table for more details")
  }

  ## ---- Summarise fit info -----------------------------------------------------
  nstudies <- paste0("study", seq(1, length(conns), 1))

  ## First we get the loglikelihood value for each study and each model
  raw_logs <- models %>%
    map(function(x) {
      nstudies %>% map(function(y) {
        tibble(
          loglik = x$output.summary[[y]]$logLik
        )
      })
    }) %>%
    map(function(x) {
      set_names(x, names(conns))
    }) %>%
    set_names(poly_names)

  ## Now we put this into a nicer format
  fit.tab <- raw_logs %>%
    map(unlist) %>%
    bind_rows(.id = "model")

  colnames(fit.tab) <- str_remove(colnames(fit.tab), ".loglik")

  ## Calculate a sum of the loglikelihoods
  fit.tab <- fit.tab %>%
    mutate(
      sum_log = rowSums(across(-model))
    ) %>%
    arrange(desc(sum_log))

  out <- list(models = models, convergence = convergence, fit = fit.tab)

  return(out)
}

#' Fit multiple mixed effects models containing different combination of 
#' fractional polynomials
#'
#' This function enables you to fit multiple models with different combinations 
#' of polynomial terms and compares the fit.
#'
#' @importFrom dsBaseClient ds.lmerSLMA
#' @importFrom purrr map flatten_chr map set_names map_lgl
#' @importFrom dplyr arrange bind_rows dense_rank group_split mutate select
#'             starts_with desc across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_detect str_remove
#' @importFrom tibble tibble
#' @importFrom DSI datashield.connections_find
#'
#' @template conns
#' @template df
#' @param formulae a vector of model formulae to fit
#' @param poly_names a vector of names for your models
#' @template checks
#' @author Tim Cadman
#'
#' @export
dh.lmeMultPoly <- function(df = NULL, formulae = NULL, poly_names = NULL, conns = NULL, checks = TRUE) {
  sum_log <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(formulae)) {
    stop("`formulae` must not be NULL.", call. = FALSE)
  }

  if (is.null(poly_names)) {
    stop("`poly_names` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, conns = conns)
  }

  loglik <- model <- study <- log_rank <- . <- av_rank <- loglik_study1 <- loglik_study2 <- NULL

  ## ---- Run the models ---------------------------------------------------------
  suppressWarnings(
    models <- formulae %>%
      map(
        ~ tryCatch(
          {
            ds.lmerSLMA(
              dataName = df,
              formula = .x,
              datasources = conns
            )
          },
          error = function(error_message) {
            out <- list("failed", error_message)
            return(out)
          }
        )
      )
  )

  names(models) <- poly_names

  ## ---- Identify models which failed completely ------------------------------
  fail_tmp <- models %>%
    map_chr(~ .[[1]][[1]][[1]] %>% str_detect("failed", negate = TRUE))

  fail_messages <- tibble(
    poly = poly_names[fail_tmp == FALSE],
    message = models[fail_tmp == FALSE] %>% map_chr(~ .[[2]]$message)
  )

  failure <- tibble(
    poly = poly_names,
    completed = fail_tmp
  ) %>%
    left_join(., fail_messages, by = "poly")

  ## ---- Identify the models with some convergence issues ---------------------
  poly_comp <- models[fail_tmp == TRUE]

  con_any <- poly_comp %>%
    map(~ .x$Convergence.error.message) %>%
    map(~ str_detect(., "no convergence error reported")) %>%
    map_lgl(function(x) {
      any(x == FALSE)
    })

  convergence <- tibble(
    poly = poly_names[fail_tmp == TRUE],
    all_converged = !con_any
  )

  problems <- left_join(failure, convergence, by = "poly")

  ## ---- Summarise convergence info ---------------------------------------------
  if (all(problems$completed == FALSE)) {
    warning("All models failed. Check 'convergence' table for more details")
  }

  if (all(problems$completed != FALSE) & any(problems$completed == FALSE)) {
    warning("Some models threw an error message. Check 'convergence' table for more details")
  }

  if (any(!is.na(problems$all_converged) & problems$all_converged == FALSE)) {
    warning("Not all models have converged for all cohorts. Check 'convergence'
            table for more details along with model output")
  }

  ## ---- Summarise fit info -----------------------------------------------------
  nstudies <- paste0("study", seq(1, length(conns), 1))

  if (length(poly_comp) > 1) {
    ## First we get the loglikelihood value for each study and each model
    raw_logs <- models[fail_tmp == TRUE] %>%
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
      set_names(poly_names[fail_tmp == TRUE])

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

    ## Add in some NAs for the models which threw errors
    fit.tab <- fit.tab %>%
      add_row(model = poly_names[fail_tmp == FALSE])
  }

  out <- list(models = models, convergence = problems, fit = fit.tab)

  return(out)
}

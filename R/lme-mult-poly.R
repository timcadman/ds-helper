<<<<<<< HEAD
#' Function to perform every combination of MLM fractional polynomials
#'
#' @param conns connection objects for DataSHIELD backends
#' @param data name of dataFrame
#' @param outcome list of outcome variables
#' @param type type of LME
#'
#' @importFrom utils combn
#' @importFrom purrr map_chr
#' @importFrom dplyr dense_rank
#'
#' @importFrom dsBaseClient ds.lmerSLMA ds.isNA
#'
#' @export
dh.lmeMultPoly <- function(conns = opals, data, outcome, type = "Normal") {
  log_lik <- NULL

  ## First arrange the data to keep it happy when fitting binomial model
  # data <- arrange(data, id)

  # Next get all combinations of polynomials. Here we have to make it work
  # so model is fit with single polynomials and combinations

  ## Vector of polynomials
  polys <- c(
    "age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5",
    "age_2", "age_3"
  )

  ## Create all combinations of these
  comb <- t(combn(combn(polys, 1, paste, collapse = ""), 2))

  ## Create terms of these for model formulas
  comb_form <- paste(comb[, 1], "+", comb[, 2])

  ## Combine these with each of the single polynomials
  comb_form <- c(comb_form, polys)

  # Finally we need to add the single polynomials to the df with the
  # combinations of polynomials. This is used later on for the table
  # of fit statistics
  comb <- rbind(comb, cbind(polys, NA))

  ## Run the models
  poly.fit <- list()
  converged <- vector()

  for (i in 1:length(comb_form)) {
    if (type == "Normal") {
      form <- paste0(
        outcome, " ~ ", "1 + ", comb_form[i], "+ ",
        "(1|child_id_int)"
      )

      est <- 0
    } else if (type == "Binomial") {
      form <- paste0(
        "logit(", outcome, ")", " ~ ", "1 + ", comb_form[i], "+ ",
        "(", comb_form[i], "| child_id_int)"
      )

      est <- 1
    }

    poly.fit[[i]] <- ds.lmerSLMA(
      dataName = data,
      formula = form,
      datasources = conns
    )


    if (poly.fit[[i]]$Convergence.error.message == "Study1: no convergence error reported") {
      poly.fit[[i]] <- poly.fit[[i]]
      converged[i] <- TRUE
    } else {
      poly.fit[[i]] <- NULL
      print("Model did not converge, coefficients not stored")
      converged[i] <- FALSE
    }
  }

  ## Display how many models removed
  removed <- comb_form[which(converged == FALSE)]

  if (length(removed) > 0) {
    cat("The following model(s) were removed due to non-convergence: ",
      removed,
      sep = "\n"
    )
  } else if (length(removed) == 0) {
    print("All models converged succesfully")
  }

  ## Remove non-converged models
  poly.fit <- poly.fit[which(converged == TRUE)]


  ## Create table with fit statistics
  fit.tab <- data.frame(matrix(NA, nrow = length(poly.fit), ncol = 3))

  colnames(fit.tab) <- c("Polynomial 1", "Polynomial 2", "log_lik")

  fit.tab[, 1:2] <- comb[which(converged == TRUE), ]
  fit.tab[, 3] <- poly.fit %>% map_chr(function(x) {
    x$output.summary$study1$logLik
  })

  ## Create a variable ranking the fit
  fit.tab %<>% mutate(log_rank = dense_rank(log_lik))

  out <- list(poly.fit, fit.tab, data, outcome)

  return(out)
}

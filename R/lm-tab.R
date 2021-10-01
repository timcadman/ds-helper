#' Extracts the coefficients and confidence intervals from a ds.glm or
#' ds.glmSLMA model.
#'
#' @param model saved output from either ds.glm,  ds.glmSLMA or ds.lmerSLMA
#' @param type either "ipd", "slma" or "lmer"
#' @param coh_names a vector of cohort names. Note this needs to be in the same
#'                  order as the cohorts provided to the model.
#' @param direction either "long" or "wide"
#' @param ci_format format for the confidence intervals when direction == "wide".
#'                  "separate" outputs separate columns with upper and lower CIs.
#'                  "paste" adds these in brackets to the coefficient.'
#' @param round_digits Number of decimal places to use in table. Default is 2.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% select
#' @importFrom rlang arg_match
#'
#' @return A tibble containing the estimate with lower and upper confidence intervals
#'
#' @export
dh.lmTab <- function(model = NULL, type = NULL, coh_names = NULL,
                     direction = c("long", "wide"), ci_format = NULL,
                     family = "gaussian", round_digits = 2) {

    Estimate <- cohort <- se <- pooled.ML <- se.ML <- value <- coefficient <- variable <- est <- NULL

  if (is.null(model)) {
    stop("Please specify a model from which to extract coefficients")
  }
  if (is.null(type)) {
    stop("Please specify which type of model was fit")
  }
  if (is.null(coh_names) & type %in% c("lmer", "slma")) {
    stop("Please provide a vector of cohort names")
  }

  type <- arg_match(type, c("ipd", "slma", "lmer"))
  direction <- arg_match(direction)
  ci_format <- arg_match(ci_format, c("paste", "separate"))
  family <- arg_match(family, c("gaussian", "binomial"))

  if (direction == "long" & ci_format == "paste") {
    warning("It is not possible to paste CIs in long format. Argument ignored")
  }

  if (family == "gaussian") {
    lowci <- "low0.95CI"
    highci <- "high0.95CI"
  } else if (family == "binomial") {
    lowci <- "low0.95CI.LP"
    highci <- "high0.95CI.LP"
  }

  if (type == "ipd") {
    out <- tibble(
      variable = dimnames(model$coefficients)[[1]],
      est = round(model$coefficients[, "Estimate"], round_digits),
      lowci = round(model$coefficients[, lowci], round_digits),
      uppci = round(model$coefficients[, highci], round_digits),
      pvalue = round(model$coefficients[, "p-value"], round_digits)
    ) %>%
      pivot_longer(
        cols = -variable,
        names_to = "coefficient",
        values_to = "value"
      )
  } else if (type == "slma" | type == "lmer") {
    nstudy <- model$num.valid.studies

    separate <- paste0("study", seq(1, nstudy, 1)) %>%
      map(function(x) {
        model$output.summary[[x]]$coefficients
      }) %>%
      set_names(coh_names) %>%
      map(~ as_tibble(x = ., rownames = "variable")) %>%
      bind_rows(.id = "cohort") %>%
      rename(est = Estimate) %>%
      rename(se = "Std. Error") %>%
      select(cohort, variable, est, se)

    slma <- model$SLMA.pooled.ests.matrix %>%
      as_tibble(rownames = "variable") %>%
      rename(est = pooled.ML) %>%
      rename(se = se.ML) %>%
      mutate(cohort = "combined") %>%
      select(cohort, variable, est, se)

    out <- bind_rows(separate, slma) %>%
      group_by(variable, cohort) %>%
      group_split() %>%
      map(
        ~ mutate(
          .data = .,
          lowci = est - 1.96 * se,
          uppci = est + 1.96 * se
        )
      ) %>%
      map(
        ~ pivot_longer(
          data = .x,
          cols = c(est, se, lowci, uppci),
          names_to = "coefficient",
          values_to = "value"
        )
      ) %>%
      bind_rows()
  }

  out <- out %>%
    mutate(
      variable = ifelse(variable == "(Intercept)", "intercept", variable),
      value = round(value, round_digits)
    ) %>%
    filter(coefficient != "se")


  if (direction == "long") {
    return(out)
  } else if (direction == "wide" & ci_format == "separate") {
    out <- out %>%
      pivot_wider(
        names_from = c(coefficient),
        values_from = value
      )

    return(out)
  } else if (direction == "wide" & ci_format == "paste" & type == "ipd") {
    out <- out %>%
      group_by(variable) %>%
      group_split() %>%
      map(function(x) {
        mutate(x, value = paste0(x$value[1], " (", x$value[2], ",", x$value[3], ")"))
      }) %>%
      map(
        ~ filter(., coefficient == "est")
      ) %>%
      bind_rows() %>%
      select(-coefficient) %>%
      rename(est = value)

    return(out)
  } else if (direction == "wide" & ci_format == "paste" & type != "ipd") {
    out <- out %>%
      group_by(cohort, variable) %>%
      group_split() %>%
      map(function(x) {
        mutate(x, value = paste0(x$value[1], " (", x$value[2], ",", x$value[3], ")"))
      }) %>%
      map(
        ~ filter(., coefficient == "est")
      ) %>%
      bind_rows() %>%
      select(-coefficient) %>%
      rename(est = value) %>%
      pivot_wider(
        names_from = variable,
        values_from = est
      )

    return(out)
  }
}

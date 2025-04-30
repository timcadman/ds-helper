#' Extracts coefficients and confidence intervals from linear models
#'
#' To conveniently view model results or make tables, it is useful to extract
#' the coefficients in a useable format. This function extracts coefficients for
#' ds.glm, ds.glmSLMA and ds.lmerSLMA objects.
#'
#' @param model Object returned from either ds.glm, ds.glmSLMA or ds.lmerSLMA
#' functions.
#' @param type Character specifying type of object provided in `model`. Can be
#' either "glm_ipd", "glm_slma" or "lmer_slma".
#' @param coh_names Character vector of cohorts included in `model`. Note this
#' must be in the same order as the cohorts were included in the model.
#' @param direction Character specifying the output format. Can be either "wide"
#' or "long". See "Value" for more details.
#' @param ci_format format for the confidence intervals when direction is "wide".
#' If "separate", upper and lower confidence intervals are displayed as columns.
#' If "paste", confidence intervals are returned in the same column as the
#' coefficient within brackets. Option "paste" is only available if `ci_format`
#' is "wide"
#' @template digits
#' @param family Specifies the family used in the analysis where type is
#' "glm_ipd" or "glm_slma". Options are "gaussian" or "binomial", with default
#' "gaussian".
#' @param exponentiate Optionally, specify whether estimates from binomial models should
#' be exponentiated, ie returned as odds ratios. This argument is ignored if
#' `type` is "gaussian".
#' @param extract_random Optionally, specify whether to return random effects in mixed effects models.
#'
#' @importFrom checkmate assert_string assert_set_equal makeAssertCollection
#' @importFrom tibble tibble
#' @importFrom dplyr mutate %>% select case_when
#' @importFrom rlang arg_match .data
#' @import lme4
#'
#' @return A tibble. When `direction` is "wide" & `ci_format` is "paste", this
#' contains five columns:
#' * variable
#' * est
#' * lowci
#' * uppci
#' * pvalue
#'
#' When `direction` is "long", a tibble with three columns is returned:
#' * variable
#' * coefficient (containing values "est", "lowci", "uppci", "pvalue")
#' * value
#'
#' When `type` is "lmer_slma", a list of 2 elements where the first element
#' is as described as above, and the second element is a tibble of random
#' effects with 5 columns: "cohort", "group", "var1", "var2", "stddev".
#' "stddev" gives either the
#'
#' #' Extracts the random effects from an lmer_slma model. Returns tibble
#' containing, for each cohort, the standard deviation of the random terms
#' and the standard deviation of the residual error. If two or more random
#' terms are included, it also returns the correlation between the terms.
#'
#'
#' @md
#' @family descriptive functions
#'
#' @export
dh.lmTab <- function(model = NULL, type = NULL, coh_names = NULL,
                     direction = NULL, ci_format = NULL,
                     family = "gaussian", digits = 2,
                     exponentiate = FALSE, extract_random = FALSE) {
  Estimate <- cohort <- se <- pooled.ML <- se.ML <- value <- coefficient <-
    variable <- est <- lowci <- uppci <- pvalue <- . <- stddev <- NULL

  lm_tab_check_args(
    model, type, direction, ci_format, family, coh_names,
    exponentiate
  )

  if (type == "glm_ipd") {
    coefs <- extract_ipd(model)
    coefs <- rename_ipd(coefs)
    coefs <- add_ns_ipd(coefs, model)
  }

  if (type %in% c("glm_slma", "lmer_slma")) {
    nstudy <- paste0("study", seq(1, model$num.valid.studies))
    coh_coefs <- extract_slma_coh(model, coh_names, nstudy)
  }

  if (type == "glm_slma") {
    coh_coefs <- rename_glm_slma(coh_coefs)
    coh_ns <- extract_ns_slma(model, nstudy)
    coh_coefs <- add_ns_slma(coh_ns, coh_coefs, coh_names)
  }

  if (type == "lmer_slma") {
    coh_coefs <- rename_lmer_slma(coh_coefs)
    coh_ns <- extract_ns_lmer(model, nstudy)
    coh_coefs <- add_ns_slma(coh_ns, coh_coefs, coh_names)

    if (extract_random) {
      random <- extract_random(model, coh_names, nstudy)
      random <- rename_intercept(random, col_name = "var1")

      random <- random %>%
        mutate(across(stddev, ~ round(., digits)))
    }
  }

  if (type %in% c("glm_slma", "lmer_slma")) {
    pooled_coefs <- extract_slma_pooled(model, nstudy)
    pooled_coefs <- rename_slma_pooled(pooled_coefs)

    pooled_coefs <- pooled_coefs %>%
      mutate(
        n_obs = sum(coh_ns),
        n_coh = length(nstudy)
      )

    coefs <- bind_rows(coh_coefs, pooled_coefs)
    coefs <- add_ci(coefs)
  }

  if (exponentiate == TRUE & family == "binomial" & direction == "long") {
    coefs <- coefs %>%
      mutate(across(c(est, lowci, uppci), ~ exp(.)))
  }

  coefs <- coefs %>%
    mutate(across(est:uppci, ~ round(., digits)))

  if (direction == "long") {
    coefs <- coefs %>%
      pivot_longer(
        cols = c(est:uppci),
        names_to = "coefficient",
        values_to = "value"
      )
  }

  if (direction == "wide" & ci_format == "paste") {
    coefs <- paste_ci(coefs)
  }

  coefs <- rename_intercept(coefs, col_name = "variable")

  if (type == "lmer_slma") {
    if (extract_random) {
      return(
        list(
          fixed = coefs,
          random = random
        )
      )
    } else {
      return(
        list(
          fixed = coefs
        )
      )
    }
  } else {
    return(coefs)
  }
}

#' Check for errors in input arguments.
#'
#' @return error message if any checks throw an error, else nothing.
#'
#' @noRd
lm_tab_check_args <- function(model, type, direction, ci_format, family,
                              coh_names, exponentiate) {
  n_studies <- NULL

  error_messages <- makeAssertCollection()

  checkmate::assert_list(model, add = error_messages)
  checkmate::assert_choice(type, c("glm_ipd", "glm_slma", "lmer_slma"), add = error_messages)
  checkmate::assert_choice(direction, c("long", "wide"), add = error_messages)
  checkmate::assert_choice(ci_format, c("paste", "separate"), add = error_messages)
  checkmate::assert_choice(family, c("gaussian", "binomial"), add = error_messages)

  if (type %in% c("glm_slma", "lmer_slma")) {
    n_studies <- model[["num.valid.studies"]]
    checkmate::assert_character(coh_names, add = error_messages)
    checkmate::assert_set_equal(length(coh_names), n_studies, add = error_messages)

    if (exponentiate == TRUE & family == "gaussian") {
      warning("It is not recommended to exponentiate coefficients from linear
            regression: argument is ignored")
    }

    if (type == "lmer_slma") {
      checkmate::assert_choice(family, "gaussian", add = error_messages)
    }
  }

  return(reportAssertions(error_messages))
}

#' Extracts the model coefficients from a glm_ipd model.
#'
#' @param model From outer function.
#' @param type From outer function.
#' @return tibble with 7 columns: "variable", "Estimate", "Std. Error",
#' "z-value", "p-value", "low0.95CI", "high0.95CI".
#' @noRd
extract_ipd <- function(model, type) {
  coefs <- as_tibble(model$coefficients, rownames = "variable")
  return(coefs)
}

#' Renames coefficients from glm_ipd model to desired output.
#'
#' @param coefs Output from extract_ipd
#' @return tibble with 6 columns: "variable", "est", "se", "pvalue", "lowci",
#' "uppci".
#' @noRd
rename_ipd <- function(coefs, family) {
  if(family == "gaussian") {
  renamed <- coefs %>%
    dplyr::select("variable",
      est = "Estimate", se = "Std. Error",
      pvalue = "p-value", lowci = "low0.95CI", uppci = "high0.95CI"
    )
  } else if(family == "binomial") {
    renamed <- coefs %>%
      dplyr::select("variable",
        est = "Estimate", se = "Std. Error",
        pvalue = "p-value", lowci = "low0.95CI.LM", uppci = "high0.95CI.LM"
      )
    }

  return(renamed)
}

#' Adds the total number of observations within the model to the tibble of
#' coefficients.
#'
#' @param coefs Tibble of coefficients
#' @param model From outer function.
#' @return tibble with extra column: "n_obs".
#' @noRd
add_ns_ipd <- function(coefs, model) {
  ns <- coefs %>%
    mutate(n_obs = model$nsubs)

  return(ns)
}

#' Extracts the model coefficients from a glm_slma model.
#'
#' @param model From outer function.
#' @param coh_names From outer function.
#' @param nstudy Vector of form "study1, study2, ... studyn" corresponding to
#' number of studies in model.
#' @return tibble with 6 columns: "cohort", "variable", "Estimate",
#' "Std. Error", "t value", "Pr(>|t|)".
#' @noRd
extract_slma_coh <- function(model, coh_names, nstudy) {
  cohort_coefs <- nstudy %>%
    map(function(x) {
      model$output.summary[[x]]$coefficients %>%
        as_tibble(rownames = "variable")
    }) %>%
    set_names(coh_names) %>%
    bind_rows(.id = "cohort")

  return(cohort_coefs)
}

#' Renames coefficients from glm_slma model to desired output.
#'
#' @param coefs Output from extract_slma_coh
#' @return tibble with 5 columns: "cohort", "variable", "est", "se", "pvalue".
#' @importFrom tidyselect contains
#' @noRd
rename_glm_slma <- function(coefs) {
  coefs_renamed <- coefs %>%
    dplyr::select("cohort", "variable",
      est = "Estimate",
      se = "Std. Error", pvalue = contains("Pr(>|")
    )

  return(coefs_renamed)
}

#' Renames coefficients from glm_slma model to desired output.
#'
#' @param coefs Output from extract_slma_coh
#' @return tibble with 6 columns: "variable", "est", "se", "pvalue", "lowci",
#' "uppci".
#' @noRd
rename_slma_pooled <- function(pooled_coefs) {
  pooled_renamed <- pooled_coefs %>%
    dplyr::select("cohort", "variable", est = "pooled.ML", se = "se.ML")

  return(pooled_renamed)
}

#' Extracts the number of observations contained within a glm_slma model for
#' each cohort.
#'
#' @param model From outer function
#' @param nstudy Vector of form "study1, study2, ... studyn" corresponding to
#' number of studies in model.
#' @return Vector of integers of length of `nstudy`.
#' @noRd
extract_ns_slma <- function(model, nstudy) {
  ns <- nstudy %>%
    map_int(function(x) {
      model$output.summary[[x]]$Nvalid
    })

  return(ns)
}

#' Adds column to tibble containing number of observations within a model
#'
#' @param ns Integer vector with model ns output from extract_ns_slma.
#' @param coefs Tibble with model coefficients.
#' @param coh_names From outer function.
#' @return Tibble with two additional columns: "n_obs", "n_coh". "n_coh" will
#' always equal one: this column is added so that the structure matches that
#' output from the pooled results where this number will vary.
#' @noRd
add_ns_slma <- function(ns, coefs, coh_names) {
  ns_tibble <- tibble(
    cohort = coh_names,
    n_obs = ns
  ) %>%
    mutate(n_coh = 1)

  ns_out <- left_join(
    coefs, ns_tibble,
    by = "cohort"
  )

  return(ns_out)
}

#' Renames coefficients from lmer_slma model to desired output.
#'
#' @param coefs Output from extract_slma_coh
#' @return tibble with 5 columns: "cohort", "variable", "est", "se", "pvalue".
#' 'pvalue' is empty because this is not currently returned from the lmer_slma
#' model.
#' @noRd
rename_lmer_slma <- function(coefs) {
  coefs_renamed <- coefs %>%
    dplyr::select("cohort", "variable", est = "Estimate", se = "Std. Error") %>%
    mutate(pvalue = NA)

  return(coefs_renamed)
}

#' Extracts the number of observations contained within a lmer_slma model for
#' each cohort.
#'
#' @param model From outer function
#' @param nstudy Vector of form "study1, study2, ... studyn" corresponding to
#' number of studies in model.
#' @return Vector of integers of length of `nstudy`.
#' @noRd
extract_ns_lmer <- function(model, nstudy) {
  ns <- nstudy %>%
    map_int(function(x) {
      model$output.summary[[x]]$devcomp$dims[["N"]]
    })

  return(ns)
}

#' Extracts the random effects from an lmer_slma model. Returns tibble
#' containing, for each cohort, the standard deviation of the random terms
#' and the standard deviation of the residual error. If two or more random
#' terms are included, it also returns the correlation between the terms.
#'
#' @param model From outer function
#' @param coh_names From outer function.
#' @param nstudy Vector of form "study1, study2, ... studyn" corresponding to
#' number of studies in model.
#' @return Tibble with 5 columns: "cohort", "group", "var1", "var2", "stddev".
#' @noRd
extract_random <- function(model, coh_names, nstudy) {
  random_extracted <- nstudy %>%
    map(function(x) {
      model$output.summary[[x]]$varcor$child_id_int %>%
        as.data.frame() %>%
        as_tibble() %>%
        dplyr::select(
          group = "grp", "var1", "var2", stddev = "sdcor"
        )
    }) %>%
    set_names(coh_names) %>%
    bind_rows(.id = "cohort")

  return(random_extracted)
}

#' Extracts the meta-analysed model coefficients from either a glm_slma or
#' lmer_slma model.
#'
#' @param model From outer function.
#' @param nstudy Vector of form "study1, study2, ... studyn" corresponding to
#' number of studies in model.
#' @return tibble with 8 columns: "variable", "pooled.ML", "se.ML",
#' "pooled.REML", "se.REML", "pooled.FE", "se.FE", "cohort".
#' @noRd
extract_slma_pooled <- function(model, nstudy) {
  pooled_coefs <- model$SLMA.pooled.ests.matrix %>%
    as_tibble(rownames = "variable") %>%
    mutate(cohort = "combined")

  if (length(nstudy) == 1) {
    pooled_coefs$variable <- rownames(model$betamatrix.all)
  }

  return(pooled_coefs)
}

#' Renames meta-analysed coefficients from slma model to desired output.
#'
#' @param coefs Output from extract_slma_pooled
#' @return tibble with 5 columns: "cohort", "variable", "est", "se".
#' @noRd
rename_slma_pooled <- function(pooled_coefs) {
  pooled_renamed <- pooled_coefs %>%
    dplyr::select("cohort", "variable", est = "pooled.ML", se = "se.ML")

  return(pooled_renamed)
}

#' Calculates upper and lower confidence intervals based on standard error and
#' adds these to tibble of coeffients.
#'
#' @param coefs Tibble of coefficients
#' @return Original tibble with additional two columns: "uppci" and "lowci".
#' @noRd
add_ci <- function(coefs) {
  . <- NULL

  ses <- coefs %>%
    mutate(
      lowci = .$est - 1.96 * .$se,
      uppci = .$est + 1.96 * .$se
    )

  return(ses)
}

#' Pastes confidence intervals to estimates.
#'
#' @param coefs tibble of coefficients containing at least "se", "lowci" and
#' "uppci"
#' @return Input tibble with (i) "est" column replaced with
#' "est (lowci, uppci)", (ii) columns "lowci" and "uppci" removed.
#' @noRd
paste_ci <- function(coefs) {
  . <- NULL

  coefs_paste <- coefs %>%
    mutate(est = paste0(.$est, " (", .$lowci, ",", .$uppci, ")")) %>%
    dplyr::select(-"lowci", -"uppci")

  return(coefs_paste)
}

#' In model output the intercept is listed a bit strangely, as "(Intercept)".
#' To improve readability this function renames to "intercept".
#'
#' @param coefs Tibble of coefficients.
#' @return Input tibble with all instances of "(Intercept)" in column "variable"
#' renamed as detailed above.
#' @noRd
rename_intercept <- function(coefs, col_name) {
  int_renamed <- coefs %>%
    mutate(
      !!sym(col_name) := ifelse(
        !!sym(col_name) == "(Intercept)", "intercept", !!sym(col_name)
      )
    )

  return(int_renamed)
}

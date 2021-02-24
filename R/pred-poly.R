
dh.predPoly <- function(df, fixed, core_agevar, coeftab, conns, type = c("linear", "nonlinear")) {

  ## ---- Make factor version of rank variable -----------------------------------
  fix_fac <- paste0(fixed, "_fac")

  ds.asFactor(paste0(df, "$", fixed), newobj = fix_fac, datasources = conns)
  ds.cbind(c(df, fix_fac), newobj = df, datasources = conns)

  ## ---- Get age info for each cohort -------------------------------------------
  ages_ref <- dh.getStats(
    df = df,
    vars = c(core_agevar, fix_fac),
    conns = conns
  )

  ## ---- Get coefficients from lmer object --------------------------------------
  coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
    filter(cohort != "coefficient")

  fixed <- ages_ref$categorical %>%
    filter(value != 0 & cohort != "combined") %>%
    group_by(cohort) %>%
    mutate(fact_levels = paste(category, collapse = ",")) %>%
    slice(1) %>%
    select(cohort, fact_levels)

  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "Combined")

  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )

  ages <- bind_rows(ages, comb_ages)

  predref <- left_join(coefs, fixed, by = "cohort") %>%
    left_join(., ages, by = "cohort")

  if (type = "age_only") {
    
    colnames(predref) <- c(
      "cohort", "intercept", "age_term_1", "age_term_2", "fact_levels", "low_age", "high_age"
    )
    
    predref %<>% mutate_at(vars(coeftab$coefficient), as.numeric)
    
    ## ---- Predict values ---------------------------------------------------------
    pred_simp <- predref %>%
      pmap(
        function(cohort, low_age, high_age, intercept, age_m_1, age_m_2) {
          agevec <- seq(low_age, high_age, by = 0.001)
          
          pred <- tibble(
            cohort = cohort,
            age = rep(agevec, 3),
            predicted = intercept + (age^-2) * age_m_2 + (age^-1) * age_m_1
          )
          
          return(pred)
        }
      )
    
    pred_s <- bind_rows(pred_simp) %>% mutate_at(vars(factor), as.factor)
    return(pred_s)
    
    } else if (type == "nonlinear") {
    colnames(predref) <- c(
      "cohort", "intercept", "age_m_2", "age_m_1", "edu_rank",
      "edu_age_m_2", "edu_age_m_1", "fact_levels", "low_age", "high_age"
    )

    predref %<>% mutate_at(vars(intercept:edu_age_m_1), as.numeric)

    ## ---- Predict values ---------------------------------------------------------
    pred_list <- predref %>%
      filter(cohort != "combined") %>%
      pmap(
        function(cohort, low_age, high_age, intercept, age_m_1, age_m_2,
                 edu_age_m_1, edu_age_m_2, fact_levels, edu_rank) {
          agevec <- seq(low_age, high_age, by = 0.001)

          pred <- tibble(
            cohort = cohort,
            age = rep(agevec, 3),
            factor = as.numeric(rep(unlist(strsplit(fact_levels, ",")), each = length(agevec))),
            predicted = intercept + (age^-2) * age_m_2 + (age^-1) * age_m_1 +
              factor * edu_rank + (age^-2) * factor * edu_age_m_2 + (age^-1) * factor * edu_age_m_1,
          )

          return(pred)
        }
      )

    pred <- bind_rows(pred_list) %>% mutate_at(vars(factor), as.factor)

    ## ---- SII and change in SII --------------------------------------------------
    sii_list <- predref %>%
      pmap(
        function(cohort, low_age, high_age, edu_age_m_1, edu_age_m_2, fact_levels,
                 edu_rank, ...) {
          agevec <- seq(low_age, high_age, by = 0.001)

          pred <- tibble(
            cohort = cohort,
            age = agevec,
            sii = edu_rank + (age^-2) * edu_age_m_2 + (age^-1) * edu_age_m_1
          )

          return(pred)
        }
      )

    sii_pred <- bind_rows(sii_list)
  } else if (type == "linear") {
    colnames(predref) <- c(
      "cohort", "intercept", "edu_rank", "age_coef", "edu_age",
      "fact_levels", "low_age", "high_age"
    )

    predref %<>% mutate_at(vars(intercept:edu_age), as.numeric)

    ## ---- Predict values ---------------------------------------------------------
    pred_list <- predref %>%
      filter(cohort != "combined") %>%
      pmap(
        function(cohort, low_age, high_age, intercept, edu_rank, age_coef,
                 edu_age, fact_levels) {
          agevec <- seq(low_age, high_age, by = 0.001)

          pred <- tibble(
            cohort = cohort,
            age = rep(agevec, 3),
            factor = as.numeric(rep(unlist(strsplit(fact_levels, ",")), each = length(agevec))),
            predicted = intercept + age * age_coef + factor * edu_rank +
              age * factor * edu_age
          )

          return(pred)
        }
      )

    pred <- bind_rows(pred_list) %>% mutate_at(vars(factor), as.factor)

    ## ---- SII and change in SII --------------------------------------------------
    sii_list <- predref %>%
      pmap(
        function(cohort, low_age, high_age, edu_age, edu_rank, ...) {
          agevec <- seq(low_age, high_age, by = 0.001)

          pred <- tibble(
            cohort = cohort,
            age = agevec,
            sii = edu_rank + age * edu_age
          )

          return(pred)
        }
      )

    sii_pred <- bind_rows(sii_list)
  }
  out <- list(
    full_model = pred,
    sii = sii_pred
  )

  return(out)
}

library(dplyr)

slma.fit <- readRDS("data/test_slma.rds")
ipd.fit <- readRDS("data/test_glm.rds")

slma_cohs <- c("alspac", "chop", "dnbc", "eden", "genr", "moba", "raine", "rhea")

################################################################################
# makeRubinTable: the pooling maths
################################################################################

## ---- Rubin's rules against hand-computed values -----------------------------
test_that("makeRubinTable applies Rubin's rules", {
  coefs <- tibble(
    variable = "age",
    cohort = "alspac",
    n_obs = 100L,
    est = c(1, 2, 3),
    se = c(0.1, 0.2, 0.3)
  )

  out <- makeRubinTable(coefs = coefs, m = 3, exponentiate = FALSE)

  # mean(c(1, 2, 3))
  expect_equal(out$pooled_mean, 2)
  # mean(c(0.1, 0.2, 0.3)^2) = 0.14 / 3
  expect_equal(out$within_var, 0.14 / 3)
  # sum((c(1, 2, 3) - 2)^2) / (3 - 1) = 2 / 2
  expect_equal(out$between_var, 1)
  # sqrt(0.14/3 + 1 + 1/3) = sqrt(1.38)
  expect_equal(out$pooled_se, sqrt(1.38))
  expect_equal(out$z_value, 2 / sqrt(1.38))
  expect_equal(out$low_ci, 2 - qnorm(0.975) * sqrt(1.38))
  expect_equal(out$upp_ci, 2 + qnorm(0.975) * sqrt(1.38))
})

## ---- Identical estimates mean no between-imputation variance ----------------
test_that("makeRubinTable returns zero between-imputation variance for identical estimates", {
  coefs <- tibble(
    variable = "age", cohort = "alspac", n_obs = 100L,
    est = c(2, 2, 2), se = c(0.5, 0.5, 0.5)
  )

  out <- makeRubinTable(coefs = coefs, m = 3, exponentiate = FALSE)

  expect_equal(out$between_var, 0)
  expect_equal(out$pooled_mean, 2)
  # With no between variance the pooled se collapses to the common se.
  expect_equal(out$pooled_se, 0.5)
})

## ---- Exponentiation applies to estimates and CIs, not to the se -------------
test_that("makeRubinTable exponentiates the estimate and CI but not the standard error", {
  coefs <- tibble(
    variable = "age", cohort = "alspac", n_obs = 100L,
    est = c(1, 2, 3), se = c(0.1, 0.2, 0.3)
  )

  plain <- makeRubinTable(coefs = coefs, m = 3, exponentiate = FALSE)
  expd <- makeRubinTable(coefs = coefs, m = 3, exponentiate = TRUE)

  expect_equal(expd$pooled_mean, exp(plain$pooled_mean))
  expect_equal(expd$low_ci, exp(plain$low_ci))
  expect_equal(expd$upp_ci, exp(plain$upp_ci))
  expect_equal(expd$pooled_se, plain$pooled_se)
})

################################################################################
# dh.pool: exponentiate for gaussian models
################################################################################

## ---- The warning promises the argument is ignored, so it must be ------------
test_that("dh.pool ignores exponentiate for gaussian models", {
  args <- list(
    imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
    family = "gaussian", coh_names = slma_cohs
  )

  plain <- do.call(dh.pool, c(args, exponentiate = FALSE))
  expd <- suppressWarnings(do.call(dh.pool, c(args, exponentiate = TRUE)))

  expect_equal(expd$pooled_mean, plain$pooled_mean)
  expect_equal(expd$low_ci, plain$low_ci)
  expect_equal(expd$upp_ci, plain$upp_ci)
})

## ---- ...and the user must still be told -------------------------------------
test_that("dh.pool warns when exponentiate is requested for gaussian models", {
  expect_warning(
    dh.pool(
      imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
      family = "gaussian", coh_names = slma_cohs, exponentiate = TRUE
    ),
    "not recommended to exponentiate"
  )
})

## ---- family is validated up front, by dh.pool, not deep inside dh.lmTab -----
test_that("dh.pool rejects an unsupported family before fitting anything", {
  err <- tryCatch(
    dh.pool(
      imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
      family = "poisson", coh_names = slma_cohs, exponentiate = TRUE
    ),
    error = function(e) e
  )

  expect_match(conditionMessage(err), "family")

  # Without an up-front check the failure surfaces from inside map(dh.lmTab),
  # wrapped by purrr as "In index: 1", which points the user at dsHelper
  # internals rather than at the argument they got wrong.
  expect_false(grepl("In index", conditionMessage(err), fixed = TRUE))
})

## ---- Binomial models must be unaffected -------------------------------------
test_that("dh.pool still exponentiates for binomial models", {
  args <- list(
    imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
    family = "binomial", coh_names = slma_cohs
  )

  plain <- do.call(dh.pool, c(args, exponentiate = FALSE))
  expd <- do.call(dh.pool, c(args, exponentiate = TRUE))

  expect_equal(expd$pooled_mean, exp(plain$pooled_mean))
})

################################################################################
# dh.pool: glm_slma
################################################################################

## ---- The regression: glm_slma must run at all -------------------------------
test_that("dh.pool runs for glm_slma", {
  expect_no_error(
    dh.pool(
      imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
      family = "gaussian", coh_names = slma_cohs, exponentiate = FALSE
    )
  )
})

## ---- One row per cohort per variable ----------------------------------------
test_that("dh.pool returns one row per cohort and variable for glm_slma", {
  out <- dh.pool(
    imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
    family = "gaussian", coh_names = slma_cohs, exponentiate = FALSE
  )

  expect_setequal(unique(out$cohort), slma_cohs)
  expect_equal(nrow(out), length(slma_cohs) * dplyr::n_distinct(out$variable))
})

## ---- SLMA's own pooled row is dropped, not pooled again ---------------------
test_that("dh.pool drops the SLMA combined row", {
  out <- dh.pool(
    imputed_glm = rep(list(slma.fit), 3), type = "glm_slma",
    family = "gaussian", coh_names = slma_cohs, exponentiate = FALSE
  )

  expect_false("combined" %in% out$cohort)
})

## ---- Pooling identical imputations must recover the single-fit estimate -----
test_that("dh.pool over identical imputations recovers the original estimates for glm_slma", {
  m <- 3
  out <- dh.pool(
    imputed_glm = rep(list(slma.fit), m), type = "glm_slma",
    family = "gaussian", coh_names = slma_cohs, exponentiate = FALSE
  )

  single <- dh.lmTab(
    model = slma.fit, type = "glm_slma", family = "gaussian",
    coh_names = slma_cohs, direction = "wide", ci_format = "separate",
    digits = 20
  ) %>%
    dplyr::filter(cohort != "combined")

  compare <- dplyr::left_join(out, single, by = c("cohort", "variable"))

  expect_equal(compare$between_var, rep(0, nrow(compare)))
  expect_equal(compare$pooled_mean, compare$est)
  expect_equal(compare$pooled_se, compare$se)
})

################################################################################
# dh.pool: glm_ipd
################################################################################

## ---- One row per variable, labelled with coh_names --------------------------
test_that("dh.pool returns one row per variable for glm_ipd", {
  out <- dh.pool(
    imputed_glm = rep(list(ipd.fit), 3), type = "glm_ipd",
    family = "gaussian", coh_names = "combined", exponentiate = FALSE
  )

  expect_equal(unique(out$cohort), "combined")
  expect_equal(nrow(out), dplyr::n_distinct(out$variable))
})

## ---- Pooling identical imputations must recover the single-fit estimate -----
test_that("dh.pool over identical imputations recovers the original estimates for glm_ipd", {
  out <- dh.pool(
    imputed_glm = rep(list(ipd.fit), 3), type = "glm_ipd",
    family = "gaussian", coh_names = "combined", exponentiate = FALSE
  )

  single <- dh.lmTab(
    model = ipd.fit, type = "glm_ipd", family = "gaussian",
    coh_names = "combined", direction = "wide", ci_format = "separate",
    digits = 20
  )

  compare <- dplyr::left_join(out, single, by = "variable")

  expect_equal(compare$between_var, rep(0, nrow(compare)))
  expect_equal(compare$pooled_mean, compare$est)
  expect_equal(compare$pooled_se, compare$se)
})

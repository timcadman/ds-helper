library(dplyr)

ipd.fit <- readRDS("data/test_glm.rds")
slma.fit <- readRDS("data/test_slma.rds")
lmer.fit <- readRDS("data/test_lmer.rds")

coh_names <- c(
  "alspac", "chop", "dnbc", "eden", "genr", "moba", "raine",
  "rhea"
)

nstudy <- paste0("study", seq(1, slma.fit$num.valid.studies))

################################################################################
# check_args
################################################################################

## ---- Incorrect: glm_ipd -----------------------------------------------------
test_that("check_args returns an error when arguments are incorrect: glm_ipd", {
  expect_error(
    lm_tab_check_args(
      model = ipd.fit, type = 99, direction = 99,
      ci_format = "asdasd", family = 99, coh_names = 99, exponentiate = 99
    ),
    "4 assertions failed:\n * Variable 'type': Must be element of set\n * {'glm_ipd','glm_slma','lmer_slma'}, but types do not match (numeric\n * != character).\n * Variable 'direction': Must be element of set {'long','wide'}, but\n * types do not match (numeric != character).\n * Variable 'ci_format': Must be element of set {'paste','separate'},\n * but is 'asdasd'.\n * Variable 'family': Must be element of set {'gaussian','binomial'},\n * but types do not match (numeric != character).",
    fixed = TRUE
  )
})

## ---- Incorrect: lmer_slma ---------------------------------------------------
test_that("check_args returns an error when arguments are incorrect: lmer_slma", {
  expect_error(
    lm_tab_check_args(
      model = slma.fit, type = "lmer_slma", direction = 99,
      ci_format = "asdasd", family = "binomial", coh_names = 99, exponentiate = 99
    ),
    "5 assertions failed:\n * Variable 'direction': Must be element of set {'long','wide'}, but\n * types do not match (numeric != character).\n * Variable 'ci_format': Must be element of set {'paste','separate'},\n * but is 'asdasd'.\n * Variable 'coh_names': Must be of type 'character', not 'double'.\n * Variable 'length(coh_names)': Must be a permutation of set {'8'},\n * but has extra elements {'1'}.\n * Variable 'family': Must be element of set {'gaussian'}, but is\n * 'binomial'.",
    fixed = TRUE
  )
})

## ---- Correct: glm_ipd -------------------------------------------------------
test_that("check_args doesn't return an error if arguments are correct: glm_slma", {
  expect_true(
    lm_tab_check_args(
      model = ipd.fit, type = "glm_ipd", direction = "wide",
      ci_format = "paste", family = "binomial",
      coh_names = coh_names, exponentiate = TRUE
    )
  )
})

## ---- Correct, lmer_slma -----------------------------------------------------
test_that("check_args doesn't return an error if arguments are correct: lmer_slma", {
  expect_true(
    lm_tab_check_args(
      model = slma.fit, type = "lmer_slma", direction = "wide",
      ci_format = "paste", family = "gaussian",
      coh_names = coh_names, exponentiate = FALSE
    )
  )
})


################################################################################
# extract_ipd
################################################################################
test_that("Check extract_ipd returns correct column names", {
  expect_equal(
    colnames(extract_ipd(ipd.fit, "glm_ipd")),
    c(
      "variable", "Estimate", "Std. Error", "z-value", "p-value", "low0.95CI",
      "high0.95CI"
    )
  )
})

test_that("Check that extract_ipd returns correct data types", {
  expect_equal(
    extract_ipd(ipd.fit, "glm_ipd") %>%
      summarise_all(class) %>%
      as.character(),
    c(
      "character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric"
    )
  )
})

################################################################################
# rename_ipd
################################################################################
ipd_rename_expected <- tibble(
  "variable" = "test",
  "Estimate" = rnorm(1),
  "Std. Error" = rnorm(1),
  "z-value" = rnorm(1),
  "p-value" = rnorm(1),
  "low0.95CI" = rnorm(1),
  "high0.95CI" = rnorm(1)
)

test_that("Check that rename_ipd returns correct column names", {
  expect_equal(
    colnames(rename_ipd(ipd_rename_expected)),
    c("variable", "est", "se", "pvalue", "lowci", "uppci")
  )
})

ipd_rename_not_expected <- ipd_rename_expected %>%
  dplyr::rename(test = variable)

test_that("rename_ipd throws error if provided incorrect variable names", {
  expect_error(
    rename_ipd(ipd_rename_not_expected)
  )
})

################################################################################
# add_ns_ipd
################################################################################
add_ns_expected <- ipd_rename_expected %>%
  mutate(n_obs = rnorm(1))

test_that("Check add_ns_ipd returns correct column names", {
  expect_equal(
    colnames(add_ns_ipd(add_ns_expected, ipd.fit)),
    c(
      "variable", "Estimate", "Std. Error", "z-value", "p-value", "low0.95CI",
      "high0.95CI", "n_obs"
    )
  )
})

test_that("Check that add_ns_ipd returns correct data types", {
  expect_equal(
    add_ns_ipd(add_ns_expected, ipd.fit) %>%
      summarise_all(class) %>%
      as.character(),
    c(
      "character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "integer"
    )
  )
})

################################################################################
# extract_slma_coh
################################################################################

## ---- Model is glm_slma ------------------------------------------------------
test_that("Check extract_slma_coh returns correct column names with glm_slma", {
  expect_equal(
    colnames(extract_slma_coh(slma.fit, coh_names, nstudy)),
    c("cohort", "variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
  )
})

test_that("Check that extract_slma_coh returns correct data types with glm_slma", {
  expect_equal(
    extract_slma_coh(slma.fit, coh_names, nstudy) %>%
      summarise_all(class) %>%
      as.character(),
    c("character", "character", "numeric", "numeric", "numeric", "numeric")
  )
})

## ---- Model is lmer_slma -----------------------------------------------------
test_that("Check extract_slma_coh returns correct column names with lmer_slma", {
  expect_equal(
    colnames(extract_slma_coh(lmer.fit, coh_names, nstudy)),
    c("cohort", "variable", "Estimate", "Std. Error", "t value")
  )
})

test_that("Check that extract_slma_coh returns correct data types with lmer_slma", {
  expect_equal(
    extract_slma_coh(lmer.fit, coh_names, nstudy) %>%
      summarise_all(class) %>%
      as.character(),
    c("character", "character", "numeric", "numeric", "numeric")
  )
})

################################################################################
# rename_glm_slma
################################################################################
slma_rename_expected <- tibble(
  "cohort" = "test",
  "variable" = "test",
  "Estimate" = 0.99,
  "Std. Error" = 0.99,
  "t value" = 0.99,
  "Pr(>|t|)" = 0.99
)

test_that("Check that rename_glm_slma returns correct column names", {
  expect_equal(
    colnames(rename_glm_slma(slma_rename_expected)),
    c("cohort", "variable", "est", "se", "pvalue")
  )
})

slma_rename_not_expected <- slma_rename_expected %>%
  dplyr::rename(test = variable)

test_that("rename_glm_slma throws error if provided incorrect variable names", {
  expect_error(
    rename_glm_slma(slma_rename_not_expected),
    "Can't subset columns that don't exist.\n✖ Column `variable` doesn't exist.",
    fixed = TRUE
  )
})

################################################################################
# extract_ns_slma
################################################################################
test_that("Check that extract_ns_slma returns vector with correct integers", {
  expect_equal(
    extract_ns_slma(slma.fit, nstudy),
    c(47434, 1088, 146239, 3349, 16432, 149598, 7173, 896)
  )
})

################################################################################
# add_ns_slma
################################################################################
add_ns_expected <- ipd_rename_expected %>%
  mutate(ns = 999)

ns_slma_expected_in <- tibble(
  cohort = coh_names,
  variable = rep("test", 8),
  est = rnorm(8),
  se = rnorm(8),
  pvalue = rnorm(8)
)

slma_ns <- rnorm(8)

ns_slma_expected_out <- ns_slma_expected_in %>%
  mutate(n_obs = slma_ns, n_coh = 1)

test_that("Check add_ns_slma returns correct column names and data", {
  expect_equal(
    add_ns_slma(slma_ns, ns_slma_expected_in, coh_names),
    ns_slma_expected_out
  )
})

################################################################################
# rename_lmer_slma
################################################################################
lmer_rename_expected <- tibble(
  "cohort" = coh_names,
  "variable" = rep("test", 8),
  "Estimate" = rnorm(8),
  "Std. Error" = rnorm(8),
  "t value" = rnorm(8)
)

test_that("Check that rename_lmer_slma returns correct column names", {
  expect_equal(
    colnames(rename_lmer_slma(lmer_rename_expected)),
    c("cohort", "variable", "est", "se", "pvalue")
  )
})

lmer_rename_not_expected <- lmer_rename_expected %>%
  dplyr::rename(test = variable)

test_that("rename_lmer_slma throws error if provided incorrect variable names", {
  expect_error(
    rename_lmer_slma(lmer_rename_not_expected),
    "Can't subset columns that don't exist.\n✖ Column `variable` doesn't exist.",
    fixed = TRUE
  )
})

################################################################################
# extract_ns_lmer
################################################################################
test_that("Check that extract_ns_lmer returns vector with correct integers", {
  expect_equal(
    extract_ns_lmer(lmer.fit, nstudy),
    c(47434, 1088, 146239, 3349, 16432, 149598, 7173, 896)
  )
})

################################################################################
# extract_random
################################################################################
# test_that("Check that extract_random returns correct column names", {
#   expect_equal(
#     colnames(extract_random(lmer.fit, coh_names, nstudy)),
#     c("cohort", "group", "var1", "var2", "stddev")
#   )
# })
#
# test_that("Check that extract_random returns correct data types", {
#   expect_equal(
#     extract_random(lmer.fit, coh_names, nstudy) %>%
#       summarise_all(class) %>%
#       as.character(),
#     c(rep("character", 4), "numeric")
#   )
# })

################################################################################
# extract_slma_pooled
################################################################################

## ---- Model is glm_slma ------------------------------------------------------
test_that("Check extract_slma_pooled returns correct column names with glm_slma
          as input", {
  expect_equal(
    colnames(extract_slma_pooled(slma.fit, nstudy)),
    c(
      "variable", "pooled.ML", "se.ML", "pooled.REML", "se.REML", "pooled.FE",
      "se.FE", "cohort"
    )
  )
})

test_that("Check that extract_slma_pooled returns correct data types with
          glm_slma as input", {
  expect_equal(
    extract_slma_pooled(slma.fit, nstudy) %>%
      summarise_all(class) %>%
      as.character(),
    c(
      "character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "character"
    )
  )
})

## ---- Model is lmer_slma -----------------------------------------------------
test_that("Check extract_slma_pooled returns correct column names with lmer_slma
          as input", {
  expect_equal(
    colnames(extract_slma_pooled(lmer.fit, nstudy)),
    c(
      "variable", "pooled.ML", "se.ML", "pooled.REML", "se.REML", "pooled.FE",
      "se.FE", "cohort"
    )
  )
})

test_that("Check that extract_slma_pooled returns correct data types with
          lmer_slma as input", {
  expect_equal(
    extract_slma_pooled(lmer.fit, nstudy) %>%
      summarise_all(class) %>%
      as.character(),
    c(
      "character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "character"
    )
  )
})

################################################################################
# rename_slma_pooled
################################################################################
pooled_rename_expected <- tibble(
  "variable" = rep("test", 5),
  "pooled.ML" = rnorm(5),
  "se.ML" = rnorm(5),
  "pooled.REML" = rnorm(5),
  "se.REML" = rnorm(5),
  "pooled.FE" = rnorm(5),
  "se.FE" = rnorm(5),
  "cohort" = rep("test2", 5)
)

test_that("Check that rename_slma_pooled returns correct column names", {
  expect_equal(
    colnames(rename_slma_pooled(pooled_rename_expected)),
    c("cohort", "variable", "est", "se")
  )
})

pooled_rename_not_expected <- pooled_rename_expected %>%
  dplyr::rename(test = variable)

test_that("rename_slma_pooled throws error if provided incorrect variable names", {
  expect_error(
    rename_slma_pooled(pooled_rename_not_expected),
    "Can't subset columns that don't exist.\n✖ Column `variable` doesn't exist.",
    fixed = TRUE
  )
})

################################################################################
# add_ci
################################################################################



################################################################################
# paste_ci
################################################################################
ci_input <- tibble(
  cohort = coh_names,
  variable = rep("test", 8),
  est = rnorm(8),
  se = rnorm(8),
  pvalue = rnorm(8),
  n_obs = rnorm(8),
  n_coh = rnorm(8),
  lowci = rnorm(8),
  uppci = rnorm(8)
)

test_that("Check that paste_ci returns correct column names", {
  expect_equal(
    colnames(paste_ci(ci_input)),
    c("cohort", "variable", "est", "se", "pvalue", "n_obs", "n_coh")
  )
})

test_that("Check that paste_ci returns correct column type", {
  expect_equal(
    class(paste_ci(ci_input)$est),
    "character"
  )
})


################################################################################
# rename_intercept
################################################################################
# intercept_in <- tibble(
#  variable = c("test", "intercept", "(Intercept)"),
#  est = rnorm(3))

# test_that("Check that rename_intercept has removed brackets", {

#  expect_equal(
#    rename_intercept(intercept_in)$variable,
#    c("test", "intercept", "intercept")
#  )
# })

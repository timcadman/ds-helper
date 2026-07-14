library(dplyr)

################################################################################
# .tidyStats
################################################################################

# `ref` is consumed by .statsTable/.statsQuantMean/.statsVar via
# group_by(variable) %>% group_map(), which orders groups by the C locale.
# .tidyStats must label the results in that same order, not by sort(), which
# uses the system locale. The two only diverge for mixed-case names.

make_ref <- function(vars, cohorts = c("server1", "server2")) {
  expand.grid(
    cohort = cohorts, variable = vars,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    select(variable, cohort) %>%
    arrange(variable)
}

# Mimics .statsTable: one element per variable in group_map order, each holding
# one tibble per cohort. The value encodes the variable it came from.
make_stats <- function(ref) {
  ref %>%
    group_by(variable) %>%
    group_map(~ pmap(., function(cohort) {
      tibble(category = "a", value = .y$variable)
    }))
}

## ---- Mixed case: the regression -------------------------------------------
test_that(".tidyStats labels stats with the variable they were computed from", {
  vars <- c("Survived", "Pclass", "Embarked", "Sex", "age_cat", "is_alone")
  ref <- make_ref(vars)

  out <- .tidyStats(ref, make_stats(ref))

  # `value` carries the variable each stat was actually computed from, so a
  # mislabelling shows up as variable != value.
  expect_equal(out$variable, out$value)
})

## ---- The orders that must agree -------------------------------------------
test_that(".tidyStats is not affected by locale-dependent sorting", {
  vars <- c("Survived", "Pclass", "Embarked", "Sex", "age_cat", "is_alone")
  ref <- make_ref(vars)

  split_order <- ref %>%
    group_by(variable) %>%
    group_split() %>%
    purrr::map_chr(~ .x$variable[[1]])

  # Guard the premise: with mixed case these differ under a non-C collation.
  # If they ever stop differing the regression above is no longer meaningful.
  skip_if(
    identical(split_order, sort(unique(vars))),
    "Collation does not distinguish group_split() order from sort() order"
  )

  out <- .tidyStats(ref, make_stats(ref))

  expect_equal(unique(out$variable), split_order)
})

## ---- All-lowercase: orders coincide, must still be right -------------------
test_that(".tidyStats labels stats correctly when names do not mix case", {
  vars <- c("survived", "pclass", "embarked", "sex", "age_cat", "is_alone")
  ref <- make_ref(vars)

  out <- .tidyStats(ref, make_stats(ref))

  expect_equal(out$variable, out$value)
})

## ---- Cohorts stay attached to their stats ---------------------------------
test_that(".tidyStats keeps cohort labels aligned with their stats", {
  vars <- c("Survived", "age_cat")
  ref <- make_ref(vars, cohorts = c("server1", "server2"))

  stats <- ref %>%
    group_by(variable) %>%
    group_map(~ pmap(., function(cohort) {
      tibble(category = "a", value = paste0(.y$variable, "-", cohort))
    }))

  out <- .tidyStats(ref, stats)

  expect_equal(out$value, paste0(out$variable, "-", out$cohort))
})

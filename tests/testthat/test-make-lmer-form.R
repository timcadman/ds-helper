# test_that("check_args returns an error when arguments are incorrect", {
#
#  expect_error(
#    check_args(outcome = 99, id_var = 99, age_vars = 99, random = "asdasd",
#               fixed = 99, age_interactions = 99),
#    "6 assertions failed:\n * Variable 'outcome': Must be of type 'string', not 'double'.\n * Variable 'outcome': Must be of type 'string', not 'double'.\n * Variable 'outcome': Must be of type 'string', not 'double'.\n * Variable 'outcome': Must be of type 'string' (or 'NULL'), not\n * 'double'.\n * Variable 'outcome': Must be of type 'string', not 'double'.\n * Variable 'outcome': Must be element of set {'intercept','slope'},\n * but is 'asdasd'.",
#    fixed = TRUE
#    )

# })

test_that("check_args doesn't return an error if arguments are correct", {
  expect_true(
    lmer_form_check_args(
      outcome = "test", id_var = "test", age_vars = "test", random = "intercept",
      fixed = "test", age_interactions = "test"
    )
  )
})

test_that("make_fixed_effects returns correct string with no interaction terms", {
  expect_identical(
    make_fixed_effects(
      age_vars = c("test_age_1", "test_age_2"),
      fixed = c("fixed_1", "fixed_2"),
      age_interactions = NULL
    ),
    "test_age_1+test_age_2+fixed_1+fixed_2"
  )
})

test_that("make_fixed_effects returns correct string with interaction terms", {
  expect_identical(
    make_fixed_effects(
      age_vars = c("test_age_1", "test_age_2"),
      fixed = c("fixed_1", "fixed_2"),
      age_interactions = "fixed_1"
    ),
    "test_age_1+test_age_2+fixed_1+fixed_2+fixed_1*test_age_1+fixed_1*test_age_2"
  )
})

#' Make formulae for fitting multiple fractional polynomial models
#'
#' This function is designed to be used with `dh.lmeMultPoly`. It generates
#' formulae for multiple fractional polynomial models which can be used as
#' input to the `formulae` argument in `dh.lmeMultPoly`.
#'
#' @param outcome Character specifying the name of outcome variable within data
#' frame to be specified in `df` argument in `dh.lmeMultPoly`.
#' @param id_var Character specifying the name of the column which uniquely
#' identifies each subject within data frame to be specified in `df`
#' argument in `dh.lmeMultPoly`.
#' @param age_vars Character vector specifying names of age polynomials present
#' in data frame specified in `df` argument in `dh.lmeMultPoly`.
#' @param random Specifies random effects to include in formulae. Use either
#' "intercept" for random intercept model or "slope" for random slope model.
#' @param fixed Optionally, character vector specifying fixed effects to be
#' included in model. If no argument is provided default is to create formula
#' only including outcome, age terms and random effects.
#' @param age_interactions Optionally, character vector specifying variable
#' provided to `fixed` for which to create interactions with `age vars`.
#' If no argument is provided default is to create formula with no interaction
#' terms.
#'
#' @return Tibble containing two columns:
#'
#' * polys = Transformations of `age_var`
#' * formula = Formula to be used as input to ds.lmerSLMA or dh.lmeMultPoly.
#'
#' @importFrom tibble tibble
#' @importFrom utils combn
#' @importFrom checkmate assert_string assert_character assert_choice reportAssertions makeAssertCollection
#'
#' @family trajectory functions
#'
#' @md
#'
#' @export
dh.makeLmerForm <- function(outcome = NULL, id_var = NULL, age_vars = NULL,
                            random = NULL, fixed = NULL, age_interactions = NULL) {
  lmer_form_check_args(outcome, id_var, age_vars, random, fixed, age_interactions)

  formula_fixed <- make_fixed_effects(age_vars, fixed, age_interactions)

  formula_random <- .make_random_effects(random, age_vars, id_var)

  out <- tibble(
    polys = c(combn(age_vars, 2, paste, collapse = ","), age_vars),
    formula = paste(
      paste0(outcome, "~1+"),
      formula_fixed, "+", formula_random
    )
  )

  return(out)
}

#' Check for errors in input arguments. All parameters inherited from outer
#' function.
#'
#' @return error message if any checks through an error, else nothing.
#'
#' @noRd
lmer_form_check_args <- function(outcome, id_var, age_vars, random, fixed, age_interactions) {
  error_messages <- checkmate::makeAssertCollection()

  checkmate::assert_string(outcome, add = error_messages)
  checkmate::assert_string(id_var, add = error_messages)
  checkmate::assert_character(age_vars, add = error_messages)
  checkmate::assert_string(random, add = error_messages)
  checkmate::assert_character(fixed, null.ok = TRUE, add = error_messages)
  checkmate::assert_string(age_interactions, null.ok = TRUE, add = error_messages)
  checkmate::assert_choice(random, c("intercept", "slope"), add = error_messages)

  return(reportAssertions(error_messages))
}

#' Translates choices for fixed effects into formula component recognised by
#' ds.lmerSLMA.
#'
#' @param age_vars inherited from dh.lmerSLMA.
#' @param fixed inherited from dh.lmerSLMA.
#' @param age_interactions inherited from dh.lmerSLMA.
#'
#' @return String representing the random effects component of the formula.
#'
#' @noRd
make_fixed_effects <- function(age_vars, fixed, age_interactions) {
  poly_fixed <- NULL

  polynomial_terms <- c(combn(age_vars, 2, paste, collapse = "+"), age_vars)

  if (!is.null(age_interactions)) {
    age_interactions <- c(
      combn(paste0(age_interactions, "*", age_vars), 2, paste, collapse = "+"),
      paste0(age_interactions, "*", age_vars)
    )
  }

  if (!is.null(fixed)) {
    additional_fixed_terms <- paste0(fixed, collapse = "+")
  }

  if (is.null(fixed) & is.null(age_interactions)) {
    all_fixed_terms <- paste(polynomial_terms, sep = "+")
  } else if (is.null(age_interactions)) {
    all_fixed_terms <- paste(
      polynomial_terms, additional_fixed_terms,
      sep = "+"
    )
  } else {
    all_fixed_terms <- paste(
      polynomial_terms, additional_fixed_terms, age_interactions,
      sep = "+"
    )
  }

  return(all_fixed_terms)
}

#' Translates choices for random effects into formula component recognised by
#' ds.lmerSLMA.
#'
#' @param random inherited from dh.lmerSLMA.
#' @param id_var inherited from dh.lmerSLMA.
#' @param poly_fixed inherited from dh.lmerSLMA.
#'
#' @return String representing the random effects component of the formula.
#'
#' @noRd
.make_random_effects <- function(random, age_vars, id_var) {
  if (random == "intercept") {
    random_eff <- paste0("(1|", id_var, ")")
  } else if (random == "slope") {
    random_eff <- paste0("(1+", age_vars, "|", id_var, ")")
  }

  return(random_eff)
}

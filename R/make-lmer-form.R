#' Make formulae for fitting multiple fractional polynomial models
#'
#' This function is designed to be used with `dh.lmeMultPoly`. It generates 
#' formulae for multiple fractional polynomial models which can be used as 
#' input to the `formulae` argument in `dh.lmeMultPoly`.
#'
#' @param outcome Outcome variable within data frame to be specified in `df` 
#' argument in `dh.lmeMultPoly`.
#' @param id_var Character specifying the name of the column which uniquely 
#' identifies each subject within data frame to be specified in `df` 
#' argument in `dh.lmeMultPoly`.
#' @param age_vars Character vector specifying names of age polynomials present
#' in data frame specified in `df` argument in `dh.lmeMultPoly`.
#' @param random Specifies random effects to include in formulae. Use either 
#' "intercept" for random intercept model or "slope" for random slope model.
#' @param fixed Optionally, character vector specifying fixed effects to be 
#' included in model.
#' @param age_interactions Logical; if TRUE, interactions terms are created 
#' between `age_vars` and `fixed`. Default is FALSE.
#'
#' @return Tibble containing two columns:
#'
#' * polys = Transformations of `age_var`
#' * formula = Formula to be used as input to ds.lmerSLMA or dh.lmeMultPoly.
#'
#' @importFrom tibble tibble
#' @importFrom utils combn
#'
#' @family trajectory functions
#'
#' @md
#'
#' @export
dh.makeLmerForm <- function(outcome = NULL, id_var = NULL, age_vars = NULL, random = NULL,
                            fixed = NULL, age_interactions = FALSE) {
  if (is.null(outcome)) {
    stop("`outcome` must not be NULL.", call. = FALSE)
  }

  if (is.null(id_var)) {
    stop("`id_var` must not be NULL.", call. = FALSE)
  }

  if (is.null(age_vars)) {
    stop("`age_vars` must not be NULL.", call. = FALSE)
  }

  random <- arg_match(random, c("intercept", "slope"))

  ## ---- Make all combinations of polynomials ---------------------------------------------------
  poly_fixed <- combn(age_vars, 2, paste, collapse = "+")


  ## ---- Define our random effects --------------------------------------------------------------

  if (random == "intercept") {
    random_eff <- paste0("(1|", id_var, ")")
  } else if (random == "slope") {
    random_eff <- paste0("(1+", poly_fixed, "|", id_var, ")")
  }

  ## ---- Do the business -----------------------------------------------------------------

  if (is.null(fixed) & !is.null(age_interactions)) {
    stop("`fixed` must not be NULL if `age_interactions` is not NULL.", call. = FALSE)
  } else if (is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", random_eff)
  } else if (!is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", random_eff)
  } else if (!is.null(fixed) & !is.null(age_interactions)) {
    fixed_tmp <- paste0(fixed, "*", age_vars)
    fixed_int <- combn(fixed_tmp, 2, paste, collapse = "+")

    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", fixed_int, "+", random_eff)
  }

  ## ---- Output -------------------------------------------------------------------
  out <- tibble(
    polys = combn(age_vars, 2, paste, collapse = ","),
    formula = forms
  )

  return(out)
}

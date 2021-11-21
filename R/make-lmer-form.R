#' Make formulae for fitting multiple fractional polynomial models
#'
#' This function is designed to be used with `dh.lmeMultPoly`. It generates 
#' formulae for multiple fractional polynomial models, which can be used as 
#' input to the `formula` argument in `dh.lmeMultPoly`.
#'
#' @param outcome outcome for the models
#' @param id_var A character giving the name of the column within `df` which 
#' uniquely identifies each subject. 
#' @param age_vars vector of names of age polynomials in dataset that you will use in your models
#' @param random either "intercept" or "slope" to specify random effects
#' @param fixed optional vector of fixed effects
#' @param age_interactions if TRUE also create interaction terms between age
#'                         terms and fixed effects
#'
#' @return a tibble containing the created formulae
#'
#' @importFrom tibble tibble
#' @importFrom utils combn
#'
#' @export
dh.makeLmerForm <- function(outcome = NULL, id_var = NULL, age_vars = NULL, random = NULL,
                            fixed = NULL, age_interactions = NULL) {
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
    formulae = forms
  )

  return(out)
}

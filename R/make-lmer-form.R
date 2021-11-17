#' Make formulae for fitting multiple fractional polynomial models
#'
#' To identify the combination of fractional polynomials which give the best
#' fit often we will fit models with lots of different combinations. This
#' function builds formulae for that purpose.
#'
#' @param outcome outcome for the models
#' @param idvar unique identifier for subject
#' @param agevars vector of names of age polynomials in dataset that you will use in your models
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
dh.makeLmerForm <- function(outcome = NULL, idvar = NULL, agevars = NULL, random = NULL,
                            fixed = NULL, age_interactions = NULL) {
  if (is.null(outcome)) {
     stop("`outcome` must not be NULL.")
  }

  if (is.null(idvar)) {
      stop("`idvar` must not be NULL.")
  }

  if (is.null(agevars)) {
     stop("`agevars` must not be NULL.")
  }

  random <- arg_match(random, c("intercept", "slope"))

  ## ---- Make all combinations of polynomials ---------------------------------------------------
  poly_fixed <- combn(agevars, 2, paste, collapse = "+")


  ## ---- Define our random effects --------------------------------------------------------------

  if (random == "intercept") {
    random_eff <- paste0("(1|", idvar, ")")
  } else if (random == "slope") {
    random_eff <- paste0("(1+", poly_fixed, "|", idvar, ")")
  }

  ## ---- Do the business -----------------------------------------------------------------

  if (is.null(fixed) & !is.null(age_interactions)) {
    stop("`fixed` must not be NULL if `age_interactions` is not NULL.")
  } else if (is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", random_eff)
  } else if (!is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", random_eff)
  } else if (!is.null(fixed) & !is.null(age_interactions)) {
    fixed_tmp <- paste0(fixed, "*", agevars)
    fixed_int <- combn(fixed_tmp, 2, paste, collapse = "+")

    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", fixed_int, "+", random_eff)
  }

  ## ---- Output -------------------------------------------------------------------
  out <- tibble(
    polys = combn(agevars, 2, paste, collapse = ","),
    formulae = forms
  )

  return(out)
}

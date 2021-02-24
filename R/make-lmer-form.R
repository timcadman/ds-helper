dh.makeLmerForm <- function(
                            outcome, idvar, agevars, random = "intercept", fixed = NULL, age_interactions = NULL) {
  random_eff <- ifelse(random == "intercept", "(1|child_id_int)")

  poly_fixed <- combn(agevars, 2, paste, collapse = "+")
  

  if (is.null(fixed) & !is.null(age_interactions)) {
    stop("You must specify fixed effects if you want to include age X fixed effects interactions")
  } else if (is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", random_eff)
  } else if (!is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", random_eff)
  } else if (!is.null(fixed) & !is.null(age_interactions)) {
    fixed_tmp <- paste0(fixed, "*", agevars)
    fixed_int <- combn(fixed_tmp, 2, paste, collapse = "+")

    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", fixed_int, "+", random_eff)
  }

  out <- tibble(
    polys = combn(agevars, 2, paste, collapse = ","),
    formulae = forms
  )

  return(out)
}

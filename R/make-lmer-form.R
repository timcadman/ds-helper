#' Make formulae for fitting multiple fractional polynomial models
#'
#' To identify the combination of fractional polynomials which give the best
#' fit often we will fit models with lots of different combinations. This
#' function builds formulae for that purpose.
#'
#' @param outcome outcome for the models
#' @param idvar unique identifier for subject
#' @param agevars vector of names of age polynomials in dataset
#' @param random either "intercept" or "slope" to specify random effects
#' @param fixed optional vector of fixed effects
#' @param age_interactions if TRUE also create interaction terms between age
#'                         terms and fixed effects#
#'
#' @return a tibble containing the created formulae
#'
#' @importFrom tibble tibble
#' @importFrom utils combn
#'
#' @export
dh.makeLmerForm <- function(
                            outcome = NULL, idvar = NULL, agevars = NULL, random = "intercept", fixed = NULL, age_interactions = NULL) {
  if (is.null(outcome)) {
    stop("Please specify the name of your outcome variable")
  }

  if (is.null(idvar)) {
    stop("Please specify the unique identifier")
  }

  if (is.null(agevars)) {
    stop("Please specify a vector of age polynomials, corresponding to variables
         in df")
  }

<<<<<<< HEAD
  random_eff <- paste0("(", 1, "|", idvar, ")")
=======
if(random == "intercept"){
random_eff <- paste0("(1|", idvar, ")")

} 
>>>>>>> 669629822e62248a4b1b961cc8813417f5aeb2ff

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

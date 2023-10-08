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
#' @importFrom checkmate assert check_string check_choice reportAssertions 
#' makeAssertCollection
#'
#' @family trajectory functions
#'
#' @md
#'
#' @export
dh.makeLmerForm <- function(outcome = NULL, id_var = NULL, age_vars = NULL,
                            random = NULL, fixed = NULL, age_interactions = NULL) {

  check_args(outcome, id_var, age_vars, random, fixed, age_interactions)
  
  formula_fixed <- make_fixed_effects(age_vars, fixed, age_interactions)
  
  formula_random <- .make_random_effects(random, age_vars, id_var)
  
  out <- tibble(
    polys = combn(age_vars, 2, paste, collapse = ","),
    formula = paste(
      paste0(outcome, "~1"), 
      formula_fixed, formula_random, sep = "+"))
  
  return(out)
  
}
  
#' Check for errors in input arguments. All parameters inherited from outer 
#' function.
#' 
#' @importFrom checkmate checkCharacter reportAssertions check_string check_choice
#
#' @return error message if any checks through an error, else nothing.
#' 
#' @noRd
check_args <- function(outcome, id_var, age_vars, random, fixed, age_interactions) {
  
  error_messages <- makeAssertCollection()
  
  assert(
    check_string(outcome), 
    check_string(id_var), 
    checkCharacter(age_vars),
    check_string(random),
    checkCharacter(fixed, null.ok = TRUE), 
    check_string(age_interactions),
    check_choice(random, c("intercept", "slope")),
    add = error_messages, 
    combine = "and")
  
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
make_fixed_effects <- function(age_vars, fixed, age_interactions){
  
  poly_fixed <- NULL
  
  polynomial_terms <- combn(age_vars, 2, paste, collapse = "+")
  
  if(!is.null(age_interactions)){
    
    age_interactions <- combn(
      paste0(age_interactions, "*", age_vars), 2, paste, collapse = "+")
    
  } 
  
  additional_fixed_terms <- paste0(fixed, collapse = "+")
  
  all_fixed_terms <- paste(
    polynomial_terms, additional_fixed_terms, age_interactions, 
    sep = "+")
  
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
.make_random_effects <- function(random, age_vars, id_var){
  
  if (random == "intercept") {
    random_eff <- paste0("(1|", id_var, ")")
    
  } else if (random == "slope") {
    random_eff <- paste0("(1+", age_vars, "|", id_var, ")")
  } 
  
  return(random_eff)
  
}

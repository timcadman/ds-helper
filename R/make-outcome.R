#' This was the original version of dh.makeStrata. It has now been deprecated.
#' 
#' dh.makeStrata contains a number of improvements: it is computationally more
#' efficient, it is more clearly coded, and it has been more thoroughly tested.
#'
#' @param df See argument `df` in `dh.makeStrata`.
#' @param outcome Renamed to `var_to_subset` in `dh.makeStrata`
#' @param age_var See argument `age_var` in `dh.makeStrata`.
#' @param bands See argument `df` in `dh.makeStrata`
#' @param mult_action See argument `mult_action` in `dh.makeStrata`
#' @param mult_vals See argument `mult_vals` in `dh.makeStrata`
#' @param keep_original Argument no longer in use in `dh.makeStrata`
#' @param df_name Renamed to `new_obj` in `dh.makeStrata`
#' @param conns See argument `conns` in `dh.makeStrata`
#' @param id_var See argument `id_var` in `dh.makeStrata`
#' @param band_action See argument `band_action` in `dh.makeStrata`
#'
#' @export
dh.makeOutcome <- function(df = NULL, outcome = NULL, age_var = NULL, bands = NULL,
  mult_action = NULL, mult_vals = NULL, df_name = NULL, id_var = "child_id", 
  band_action = NULL, keep_original = NULL, conns = NULL) {
  if (!missing(keep_original)) {
    warning("`keep_original` is no longer in use and has been ignored")
  }

  .Deprecated("dh.makeStrata")
  message("dh.makeOutcome will be defunct from version 1.0.0 onwards")

  dh.makeStrata(
    df = df,
    var_to_subset = outcome,
    age_var = age_var,
    bands = bands,
    mult_action = mult_action,
    mult_vals = mult_vals,
    new_obj = df_name,
    conns = conns,
    id_var = id_var,
    band_action = band_action
  )
}

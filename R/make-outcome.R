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
#' @name dh.makeOutcome-defunct
#' @usage dh.meanByAge(df, outcome, age_var, bands, mult_action, mult_vals,
#' keep_original, df_name, conns, id_var, band_action)
#' @keywords internal
#' @noRd
NULL

#' @rdname dsHelper-defunct
#' @section \code{dh.makeOutcome}:
#' For \code{dh.makeOutcome}, use \code{\link{dh.makeStrata}}.
#'
#' @export
dh.makeOutcome <- function(...) {
  .Defunct(msg = "`dh.makeOutcome` has been removed from this package.
    Use `dh.makeStrata` instead. See help('Defunct')")
}

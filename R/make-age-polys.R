#' Produces multiple transformations of the age term for fractional polynomial
#' analyses
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' When we do trajectory analyses using fractional polynomials we often want
#' to try models with different combinations of age polynomials. This function
#' creates multiple transformations of an age variable to different powers. We recommend moving to 
#' \code{dsTidyverseClient::ds.mutate()} which is quicker, although would require a few more lines 
#' of code.
#'
#' @template conns
#' @template df
#' @param age_var Character specifying age variable within `df` to transform.
#' @param poly_form Character vector of powers by which to transform `age_var`.
#' @param poly_names Character vector of names for the created variables. Must
#' the same length and order as `poly_form`.
#' @template checks
#' @param agevars Retired argument name. Please use `age_var' instead.
#'
#' @return Transformations of age are added as columns to server-side object
#' specified in `df`.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr pmap cross2 map_chr
#' @importFrom dsBaseClient ds.cbind
#' @importFrom DSI datashield.connections_find
#'
#' @family trajectory functions
#' @family data manipulation functions
#'
#' @export
dh.makeAgePolys <- function(df = NULL, age_var = NULL,
                            poly_form = c("^-2", "^-1", "^-0.5", "log", "^0.5", "^2", "^3"),
                            poly_names = c("_m_2", "_m_1", "_m_0_5", "log", "_0_5", "_2", "_3"),
                            conns = NULL, checks = TRUE, agevars = NULL) {
  lifecycle::signal_stage("superseded", "dh.makeAgePolys()")
  df <- df
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(age_var)) {
    stop("`age_var` must not be NULL.", call. = FALSE)
  }

  if (length(poly_names) != length(poly_form)) {
    stop("The vectors supplied to `poly_names` and `poly_form` are not the same length", call. = FALSE)
  }

  if (!missing(agevars)) {
    warning("Please use `age_var` instead of `agevars`")
    age_var <- agevars
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = age_var, conns = conns)
  }

  ## We have to do log a bit more differently
  log_yn <- any(str_detect(poly_form, "log") == TRUE)

  if (log_yn == TRUE) {
    poly_names <- poly_names[str_detect(poly_names, "log") == FALSE]
    poly_form <- poly_form[str_detect(poly_form, "log") == FALSE]
  }
  df_age <- paste0(df, "$", age_var)

  polys <- tibble(
    poly = cross2(age_var, poly_names) %>% map_chr(paste, sep = "", collapse = ""),
    form = cross2(df_age, poly_form) %>% map_chr(paste, sep = "", collapse = "")
  )

  if (log_yn == TRUE) {
    polys <- add_row(
      polys,
      poly = paste0(age_var, "_log"),
      form = paste0("log(", df_age, ")")
    )
  }

  polys %>%
    pmap(function(poly, form, ...) {
      ds.assign(
        toAssign = form,
        newobj = poly,
        datasources = conns
      )
    })

  ds.cbind(x = c(df, polys$poly), newobj = df, datasources = conns)

  dh.tidyEnv(
    obj = polys$poly,
    type = "remove"
  )

  cat("\nThe following transformations of age have been created in
    dataframe:", df, "\n\n", polys$poly)
}

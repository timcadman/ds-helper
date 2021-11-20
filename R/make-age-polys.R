#' Produces multiple transformations of age term for fractional polynomial
#' analysis.
#'
#' When we do trajectory analyses using fractional polynomials we often want
#' to try models with different combinations of age polynomials. This function
#' creates the following age transformations in datashield: age^-2, age^-1,
#' age^-0.5, log(age), age^0.5, age^2, age^3.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df opal dataframe
#' @param age_var the age variable to transform
#' @param poly_form a vector of powers by which to transform the age variable
#' @param poly_names a vector of names for the created variables, the same length
#' and order as poly_form
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#' @param agevars Retired argument name. Please use `new_obj' instead.
#'
#' @return transformations of age created in df
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr pmap cross2 map_chr
#' @importFrom dsBaseClient ds.cbind
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.makeAgePolys <- function(df = NULL, age_var = NULL, conns = NULL,
                            poly_form = c("^-2", "^-1", "^-0.5", "log", "^0.5", "^2", "^3"),
                            poly_names = c("_m_2", "_m_1", "_m_0_5", "log", "_0_5", "_2", "_3"),
                            checks = TRUE, agevars = NULL) {
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

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  ## We have to do log a bit more differently
  log_yn <- any(str_detect(poly_form, "log") == TRUE)

  if (log_yn == TRUE) {
    poly_names <- poly_names[str_detect(poly_names, "log") == FALSE]
    poly_form <- poly_form[str_detect(poly_form, "log") == FALSE]
  }
  df_age <- c(paste0(df, "$", age_var))

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

  ds.cbind(x = c(df, polys$poly), newobj = df)

  cat("\nThe following transformations of age have been created in
    dataframe:", df, "\n\n", polys$poly)
}

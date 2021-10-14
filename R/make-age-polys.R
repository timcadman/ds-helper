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
#' @param agevars the age variable to transform
#' @param poly_form a vector of powers by which to transform the age variable
#' @param poly_names a vector of names for the created variables, the same length
#' and order as poly_form
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
dh.makeAgePolys <- function(df = NULL, agevars = NULL, conns = NULL, 
  poly_form = c("^-2", "^-1", "^-0.5", "log", "^0.5", "^2", "^3"),
  poly_names = c("m_2", "m_1", "m_0_5", "log", "0_5", "2", "3")) {
  if (is.null(df)) {
    stop("Please specify a data frame which contains age variable(s)")
  }

  if (is.null(agevars)) {
    stop("Please specify one or more age variables to transform")
  }

if(length(poly_names) != length(poly_form)){

stop("The vectors supplied to arguments 'poly_names' and 'poly_form
  are not the same length")

}

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

## We have to do log a bit more differently
log_yn <- any(str_detect(poly_form, "log") == TRUE)
  
if(log_yn == TRUE){

poly_names <- poly_names[str_detect(poly_names, "log") == FALSE]
poly_form <- poly_form[str_detect(poly_form, "log") == FALSE]

}  
  df_age <- c(paste0(df, "$", agevars))

  polys <- tibble(
    poly = cross2(agevars, poly_names) %>% map_chr(paste, sep = "", collapse = ""),
    form = cross2(df_age, poly_form) %>% map_chr(paste, sep = "", collapse = "")
  )

if(log_yn == TRUE){

  polys <- add_row(
    polys, 
    poly = paste0("log", agevars), 
    form = paste0("log(", df_age, ")"))
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

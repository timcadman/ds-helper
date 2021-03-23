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
#'
#' @return transformations of age created in df
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>% 
#' @importFrom purrr pmap cross2 map_chr
#' @importFrom dsBaseClient ds.cbind 
#' 
#' @export
dh.makeAgePolys <- function(df, agevars, conns = NULL) {
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  
  poly_names <- c("m_2", "m_1", "m_0_5", "log", "0_5", "2", "3")

  poly_form <- c("^-2", "^-1", "^-0.5", "^0", "^0.5", "^2", "^3")

  df_age <- c(paste0(df, "$", agevars))

  polys <- tibble(
    poly = cross2(agevars, poly_names) %>% map_chr(paste, sep = "", collapse = ""),
    form = cross2(df_age, poly_form) %>% map_chr(paste, sep = "", collapse = "")
  )

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

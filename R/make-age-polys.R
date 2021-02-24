dh.makeAgePolys <- function(df, agevars, conns) {
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

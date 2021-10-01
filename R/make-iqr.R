#' Transforms variables based on their interquartile range
#'
#' This function is used to scale variables by the interquartile range
#' calulcated either within cohort or using the pooled IQR across cohorts.
#' The formula is: value(subject) / (75th percentile - 25th percentile).
#'
#' @param df datashield dataframe
#' @param vars variables to transform
#' @param type either "separate" to transform the variable based on the IQR
#'             calculated within cohort, or "pooled" to transform based on the
#'             pooled IQR across all cohorts provided in the 'conns' argument.
#' @param conns connections object to DataSHIELD backends
#' @param new_df_name name for new dataframe with original vars and iqr versions.
#'
#' @return the original dataframe with transformed variables added with the
#'         suffix "_iqr_c" (if cohort range was used) or "iqr_p" if pooled
#'         range was used.
#'
#' @importFrom dsBaseClient ds.colnames ds.dataFrame ds.make ds.class ds.mean
#'             ds.quantileMean
#' @importFrom purrr map pmap map_depth
#' @importFrom dplyr %>% bind_rows
#' @importFrom DSI datashield.connections_find
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble tibble
#'
#' @export
dh.makeIQR <- function(df = NULL, vars = NULL, type = c("separate", "pooled"),
                       conns = NULL, new_df_name = df) {

  . <- V1 <- cohort <- formula <- variable <- NULL

  dh.doVarsExist(df = df, vars = vars, conns = conns)

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(vars)) {
    stop("Please specify variable(s) to transform")
  }

  type <- match.arg(type)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  df_vars <- paste0(df, "$", vars)

  ## ---- Checks -----------------------------------------------------------------
  check_cont <- df_vars %>%
    map(
      ~ ds.class(., datasources = conns)
    )

  if (
    all(
      str_detect(unlist(check_cont), "numeric|integer")
    ) == FALSE) {
    stop("Can only calculate IQR for continous variables: please check class of
         provided vars")
  }

  check_na <- df_vars %>%
    map(~ ds.mean(., datasources = conns)) %>%
    map(~ .[["Mean.by.Study"]][, "EstimatedMean"]) %>%
    map(~ any(is.na(.)))

  names(check_na) <- vars

  any_na <- check_na[check_na == TRUE]

  if (length(any_na) > 0) {
    stop(paste0("The following variables have missing values for at least 1
                cohort: \n\n", names(any_na)))
  }

  ## ---- Calculate IQRs ---------------------------------------------------------
  if (type == "separate") {
    meds <- df_vars %>%
      map(
        ~ ds.quantileMean(
          x = .,
          type = "split",
          datasources = conns
        )
      )

    names(meds) <- vars

    iqr <- meds %>% map_depth(., 2, ~ .[["75%"]] - .[["25%"]])

    iqr_to_make <- iqr %>%
      map(unlist) %>%
      map(as.matrix) %>%
      map(as.data.frame) %>%
      map(as_tibble, rownames = "cohort") %>%
      bind_rows(.id = "variable") %>%
      mutate(formula = paste0(df, "$", variable, "/", V1)) %>%
      select(variable, cohort, formula)

    iqr_to_make %>%
      pmap(function(variable, cohort, formula) {
        ds.make(
          toAssign = formula,
          newobj = paste0(variable, "_iqr_c"),
          datasources = conns[cohort]
        )
      })

    ds.dataFrame(
      x = c(df, paste0(vars, "_iqr_c")),
      newobj = new_df_name,
      datasources = conns
    )
  } else if (type == "pooled") {
    meds <- df_vars %>%
      map(
        ~ ds.quantileMean(
          x = .,
          type = "combine",
          datasources = conns
        )
      )

    iqr <- meds %>% map(~ .[["75%"]] - .[["25%"]])

    names(iqr) <- vars

    iqr_to_make <-
      tibble(
        variable = names(iqr),
        formula = paste0(df, "$", variable, "/", unlist(iqr))
      )

    iqr_to_make %>%
      pmap(function(variable, formula) {
        ds.make(
          toAssign = formula,
          newobj = paste0(variable, "_iqr_p"),
          datasources = conns
        )
      })
  }

  ds.dataFrame(
    x = c(df, paste0(vars, "_iqr_p")),
    newobj = new_df_name,
    datasources = conns
  )
}

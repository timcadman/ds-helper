#' Transforms variables based on their interquartile range
#'
#' This function scales variables by their interquartile range. IQR is
#' calulcated either within cohort or using the pooled IQR across cohorts.
#' The formula used is: value[subject] / (75th percentile - 25th percentile).
#'
#' @template df
#' @param vars Character vector of columns within `df` to transform.
#' @param type Use "separate" to transform the variable based on the
#' within-cohort IQR, or "pooled" to use the pooled IQR across all cohorts
#' specified in `conns`.
#' @template conns
#' @template new_obj
#' @template checks
#' @param new_df_name Retired argument. Please use `new_obj' instead.
#'
#' @return Server-side object specified in `df` with transformed variables added
#' as columns. Variables have suffic "_iqr_c" if type is "separate" and suffix
#' "iqr_p" if pooled is type is "pooled".
#'
#' @importFrom dsBaseClient ds.colnames ds.dataFrame ds.make ds.class ds.mean
#'             ds.quantileMean
#' @importFrom purrr map pmap map_depth
#' @importFrom dplyr %>% bind_rows
#' @importFrom DSI datashield.connections_find
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble tibble
#'
#' @family data manipulation functions
#'
#' @export
dh.makeIQR <- function(df = NULL, vars = NULL, type = c("separate", "pooled"),
                       new_obj = df, conns = NULL, checks = TRUE,
                       new_df_name = NULL) {
  . <- variable <- cohort <- formula <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (!missing(new_df_name)) {
    warning("Please use `new_obj` instead of `new_df_name`")
    new_obj <- new_df_name
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  type <- match.arg(type)

  df_vars <- paste0(df, "$", vars)

  ## ---- Checks -----------------------------------------------------------------
  check_cont <- df_vars %>%
    map(~ datashield.aggregate(conns, call("classDS", .)))

  if (
    all(
      str_detect(unlist(check_cont), "numeric|integer")
    ) == FALSE) {
    stop("Can only calculate IQR for continous variables: please check class variables specified in `vars`", call. = FALSE)
  }

  ## ---- Calculate IQRs ---------------------------------------------------------
  if (type == "separate") {
    meds <- df_vars %>%
      map(function(x) {
        cally1 <- paste0("quantileMeanDS(", x, ")")
        datashield.aggregate(conns, as.symbol(cally1))
      }) %>%
      set_names(vars)

    iqr <- meds %>% map_depth(., 2, ~ .[["75%"]] - .[["25%"]])

    iqr_to_make <- iqr %>%
      map(unlist) %>%
      map(as.matrix) %>%
      map(as.data.frame) %>%
      map(as_tibble, rownames = "cohort") %>%
      bind_rows(.id = "variable") %>%
      mutate(formula = case_when(
        is.na(V1) ~ paste0(df, "$", variable),
        !is.na(V1) ~ paste0(df, "$", variable, "/", V1)
      )) %>%
      select(variable, cohort, formula)

    iqr_to_make %>%
      pmap(function(variable, cohort, formula) {
        datashield.assign(conns, paste0(variable, "_iqr_c"), as.symbol(formula))
      })

    ds.dataFrame(
      x = c(df, paste0(vars, "_iqr_c")),
      newobj = new_obj,
      datasources = conns,
      DataSHIELD.checks = FALSE,
      check.names = FALSE
    )
  } else if (type == "pooled") {

    ## ---- Identify cohorts which are all missing -----------------------------
    missing <- expand.grid(vars, names(conns)) %>%
      set_names(c("variable", "cohort")) %>%
      pmap(function(variable, cohort) {
        cally <- paste0("isNaDS(", df, "$", variable, ")")
        datashield.aggregate(conns[cohort], as.symbol(cally))
      }) %>%
      set_names(rep(vars, times = length(names(conns))))

    missing.tib <- missing %>%
      bind_rows(.id = "variable") %>%
      pivot_longer(
        cols = -variable,
        names_to = "cohort",
        values_to = "missing"
      ) %>%
      dplyr::filter(!is.na(missing))

    ## ---- Create reference tibble of variables to create  --------------------
    formean <- missing.tib %>%
      dplyr::filter(missing == FALSE) %>%
      group_by(variable)

    formean <- formean %>%
      group_split() %>%
      map(~ .$cohort) %>%
      set_names(unlist(group_keys(formean)))

    ## ---- Get pooled IQR for non-missing cohorts -----------------------------
    meds <- formean %>%
      imap(
        ~ ds.quantileMean(
          x = paste0(df, "$", .y),
          type = "combine",
          datasources = conns[.x]
        )
      )

    iqr <- meds %>%
      map_df(~ .[["75%"]] - .[["25%"]]) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "iqr"
      )

    ## ---- Reference tibble of IQR variables to create ------------------------

    ## If original variable was NA, new variable will also be NA
    iqr_to_make <- formean %>%
      map(~ tibble(cohort = .)) %>%
      bind_rows(.id = "variable") %>%
      left_join(., iqr, by = "variable") %>%
      mutate(formula = paste0(df, "$", variable, "/", iqr))

    ## Add in missing variables
    full_vars <- missing.tib %>%
      select(variable, cohort) %>%
      left_join(., iqr_to_make, by = c("variable", "cohort")) %>%
      mutate(formula = ifelse(is.na(formula), paste0(df, "$", variable), formula))

    full_vars %>%
      pmap(function(cohort, variable, formula, ...) {
        datashield.assign(
          conns[cohort], paste0(variable, "_iqr_p"), as.symbol(formula)
        )
      })

    ds.dataFrame(
      x = c(df, paste0(vars, "_iqr_p")),
      newobj = new_obj,
      datasources = conns,
      DataSHIELD.checks = FALSE,
      check.names = FALSE
    )
  }
  cat("\nThe following IQR transformations have been created in dataframe ", "'", new_obj, "':", "\n\n", sep = "")
  iqr_to_make %>%
    dplyr::select(variable, cohort) %>%
    print()
}

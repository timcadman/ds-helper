#' Calculate mean values of one repeated measures variable by an age variable
#'
#' @importFrom DSI datashield.connections_find datashield.assign
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows %>% slice
#'
#' @param conns connection object for DataSHIELD backends
#' @param df datashield dataframe
#' @param outcome outcome variable in long format
#' @param age_var age in years
#' @param intervals table defining our age bands
#' @param checks Boolean. Whether or not to perform checks prior to running function. Default is TRUE.
#'
#' @return Mean values for each unit of your age variable are returned
#'
#' @export
dh.meanByAge <- function(df = NULL, outcome = NULL, age_var = NULL, conns = NULL,
                         intervals = NULL) {
  value <- op <- tmp <- varname <- new_df_name <- age <- group <- cohort <- . <- NULL

  if (is.null(df)) {
      stop("`df` must not be NULL.")
  }

  if (is.null(outcome)) {
      stop("`outcome` must not be NULL.")
  }

  if (is.null(age_var)) {
      stop("`age_var` must not be NULL.")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if(checks == TRUE){
  dh.doesDfExist(conns = conns, df = df)
  dh.doVarsExist(conns = conns, vars = c(outcome, age_var), df = df)
}

  ## There is an easy way and a hard way. If we bin based on integer units of the
  ## binning variable it is quite quick

  if (is.null(intervals)) {


    ## ---- First we round up the age variable -----------------------------------------------
    DSI::datashield.assign(conns, "age_tmp", as.symbol(paste0(df, "$", age_var, "+0.5")))

    calltext <- call("asIntegerDS", "age_tmp")
    DSI::datashield.assign(conns, "age_round", calltext)

    ## ---- Now we get the mean values for the outcome by age --------------------------------
    calltext <- paste0("meanSdGpDS(", paste0(df, "$", outcome), ",", "age_round", ")")
    mean_tmp <- DSI::datashield.aggregate(conns, as.symbol(calltext))

    ## ---- Now put into neat long format -------------------------------------------------------
    out <- mean_tmp %>%
      map(function(x) {
        x$Mean_gp %>%
          as_tibble(rownames = "age") %>%
          mutate(age = as.numeric(str_remove(age, "age_round_")))
      }) %>%
      bind_rows(.id = "cohort") %>%
      dplyr::rename(mean = value)

    ## ---- Remove temporary objects ------------------------------------------------------------
    dh.tidyEnv(
      obj = c("age_tmp", "age_round"),
      type = "remove"
    )
  } else if (!is.null(intervals)) {

    ## ---- This is the harder one -------------------------------------------------------------------

    ## First we need to create a table defining our age bands.
    pairs <- split(intervals, ceiling(seq_along(intervals) / 2))

    subnames <- unlist(
      pairs %>% map(~ paste0("grp", "_", paste0(., collapse = "_"))),
      use.names = FALSE
    )

    cats <- tibble(
      varname = rep(subnames, each = 2),
      value = intervals,
      op = rep(c(">=", "<="), times = (length(intervals) / 2)),
      tmp = ifelse(op == ">=", "gte", "lte"),
      new_df_name = paste0("grp", tmp, value)
    )


    ## Now we create vectors of 1s and 0s indicating whether the above criteria are
    ## met for each subject.
    cats %>%
      pmap(function(value, op, new_df_name, ...) {
        ds.Boole(
          V1 = "data$age",
          V2 = value,
          Boolean.operator = op,
          newobj = new_df_name,
          datasources = conns
        )
      })

    suppressMessages(
      assign_conditions <- cats %>%
        group_by(varname) %>%
        summarise(condition = paste(new_df_name, collapse = "*"))
    )

    assign_conditions %>%
      pmap(function(condition, varname) {
        DSI::datashield.assign(conns, varname, as.symbol(condition))
      })

    ## We then use these vectors to summarise mean observed height at the age periods
    ## we are interested in.
    obs_by_agecat_comb <- assign_conditions %>%
      pull(varname) %>%
      map(
        ~ ds.meanSdGp(
          x = "data$weight",
          y = .,
          type = "split",
          datasources = conns
        )
      )

    ## Now we take these values and put them into a neater table
    out <- obs_by_agecat_comb %>%
      map(function(x) {
        x$Mean_gp %>%
          as_tibble(rownames = "group") %>%
          slice(2)
      }) %>%
      bind_rows() %>%
      mutate(group = str_remove(group, "_[^_]+$")) %>%
      mutate(group = str_remove(group, "grp_")) %>%
      pivot_longer(
        cols = -group,
        names_to = "cohort",
        values_to = "mean"
      ) %>%
      dplyr::select(cohort, group, mean)


    ## ---- Remove temporary objects -------------------------------------------------------------------
    dh.tidyEnv(
      obj = c(cats$new_df_name, assign_conditions$varname),
      conns = conns
    )
  }

  return(out)
}

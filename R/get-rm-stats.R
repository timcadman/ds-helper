#' Produces descriptive statistics based on repeated measures data
#' which it would be useful to report in papers.
#'
#' @importFrom dplyr %>% mutate across
#' @importFrom dsBaseClient ds.summary ds.asFactorSimple ds.tapply.assign ds.tapply
#'
#' @param df datashield dataframe
#' @param outcome name of outcome variable in df
#' @param id_var name of id variable in df
#' @param age_var name of age variable in df
#' @param conns connection object for DataSHIELD backends
#'
#' @return a tibble containing the following columns:
#'
#' min_age: 5th percentile of age
#' max_age: 95th percentile of age
#' n_obs: total number of observations in data
#' n_participants: total number of unique participants
#' n_meas_5: 5th percentile of measurements per individual
#' n_meas_med: median number of measurements per individual
#' n_meas_95: 95th percentile of measurements per individual
#'
#' @export
dh.getRmStats <- function(df = NULL, outcome = NULL, id_var = NULL, age_var = NULL, conns = NULL) {
  . <- n_meas_5 <- n_meas_95 <- n_meas_med <- variable <- perc_5 <- perc_95 <- cohort <- min_age <-
    max_age <- valid_n <- NULL

  if (is.null(df)) {
    stop("Please provide the name of a datashield dataframe")
  }

  if (is.null(outcome)) {
    stop("Please provide the name of your outcome variable")
  }

  if (is.null(id_var)) {
    stop("Please provide the name of id variable in df")
  }

  if (is.null(age_var)) {
    stop("Please provide the name of your age variable in df")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  ## ---- First get overall stats for some of the easy ones -------------------------------------------
  stats <- dh.getStats(
    df = df,
    vars = c(outcome, age_var),
    conns = conns
  )

  ## ---- Age range of participants -------------------------------------------------------------------
  age_ranges <- stats$continuous %>%
    dplyr::filter(variable == age_var) %>%
    mutate(
      min_age = perc_5,
      max_age = perc_95
    ) %>%
    dplyr::select(cohort, min_age, max_age)

  ## ---- Total number of outcome measurements -------------------------------------
  outcome_n <- stats$continuous %>%
    dplyr::filter(variable == outcome) %>%
    dplyr::select(cohort, n_obs = valid_n)


  ## ---- Total number of unique participants ----------------------------------------

  # First, we use ds.tapply.assign to summarise the number of observations for each
  # subject. The length of this created object then gives us the number of subjects.

  ds.asFactorSimple(paste0(df, "$", id_var), "id_fact", datasources = conns)

  ds.tapply.assign(
    X.name = paste0(df, "$", outcome),
    INDEX.names = "id_fact",
    FUN.name = "N",
    newobj = "id_summary",
    datasources = conns
  )

  n_subjects <- DSI::datashield.aggregate(conns, call("lengthDS", "id_summary$N")) %>%
    setNames(names(conns)) %>%
    bind_rows() %>%
    mutate(combined = rowSums(.)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "cohort",
      values_to = "n_participants"
    )

  ## ---- Median number of weight measurements per child ----------------------------------------

  # We can use the ds.quantileMean function with the object we created above to get the
  # median number of measurements per child.

  ds.asNumeric("id_summary$N", "id_summary_num", datasources = conns)

  quants <- DSI::datashield.aggregate(conns, as.symbol("quantileMeanDS(id_summary_num)"))

  weight_med_iqr <- quants %>%
    bind_rows(.id = "cohort") %>%
    select(cohort, "5%", "50%", "95%") %>%
    rename(n_meas_med = "50%", n_meas_5 = "5%", n_meas_95 = "95%")

  ## Get the combined version using weighted sum
  lengths <- DSI::datashield.aggregate(conns, call("lengthDS", "id_summary_num"))
  numNAs <- DSI::datashield.aggregate(conns, "numNaDS(id_summary_num)")

  valid_n <- list(lengths, numNAs) %>% pmap(~ .x - .y)

  weights <- unlist(valid_n) / sum(unlist(valid_n))

  weighted_quant <- list(quants, weights) %>% pmap(~ .x * .y)

  sum_quant <- weighted_quant %>%
    pmap(function(...) {
      sum(c(...))
    }) %>%
    bind_rows() %>%
    rename(n_meas_med = "50%", n_meas_5 = "5%", n_meas_95 = "95%") %>%
    mutate(cohort = "combined") %>%
    select(cohort, n_meas_med, n_meas_5, n_meas_95)

  quant_out <- bind_rows(weight_med_iqr, sum_quant)

  ## ---- Create final output -------------------------------------------------------------------
  out <- left_join(age_ranges, outcome_n, by = "cohort") %>%
    left_join(., n_subjects, by = "cohort") %>%
    left_join(., quant_out, by = "cohort")

  return(out)
}

#' Produces a range of descriptive statistics in a useful format
#'
#' Whilst dsBaseClient provides the functions 'ds.table' and 'ds.summary' to
#' calculate descriptive statistics, their output is not in a very useable
#' format. This function extracts key descriptive statistics and returns them in
#' tibble.
#'
#' This function also overcomes an issue with ds.summary, where it throws an
#' error if the variable is missing in one or more study. By contrast,
#' dh.getStats will return the variable for that cohort with all NAs. See
#' 'value' for details of returned statistics.
#'
#' @template conns
#' @template df
#' @param vars Character vector of columns within `df` to summarise.
#' @template digits
#' @template checks
#'
#' @family descriptive functions
#'
#' @return A client-side list with two elements: "categorical" and "continuous".
#' Each element contains a tibble with descriptive statistics as follows.
#'
#' Categorical: \cr
#' * "variable" = Variable name.
#' * cohort = Cohort name, where "combined" refers to pooled statistics.
#' * category = Level of variable, including 'missing' as a category.
#' * value = Number of observations within category.
#' * cohort_n = Total number of observations per cohort within `df`.
#' * valid_n = Number of valid observations for variable (sum of ns for all
#'            categories excluding missing).
#' * missing_n = Number of missing observations.
#' * perc_valid = Numnber of observations within a category as percentage of
#' valid_n.
#' * perc_total = Number of observations within a category as percentage of
#' cohort_n.
#'
#' Continuous: \cr
#'
#' * variable = As above.
#' * cohort = As above.
#' * mean = Mean. The pooled value calculated by fixed-effect meta-analysis.
#' * std.dev = Standard deviation. The pooled value is also calculate by fixed-
#' * effect meta-analysis.
#' * perc_5, perc_10, perc_25, perc_50, perc_75, perc_90, perc_95 = 5th to 95th
#' * percentile values.
#' * valid_n = As above.
#' * cohort_n = As above.
#' * missing_n = As above.
#' * missing_perc = Percentage of observations missing.
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% arrange group_by group_map summarise summarize ungroup
#' left_join bind_rows rename filter mutate_at vars distinct add_row n_distinct
#' @importFrom purrr map flatten_dbl pmap pmap_chr
#' @importFrom tidyr replace_na
#' @importFrom dsBaseClient ds.length ds.dim ds.levels
#' @importFrom stringr str_detect
#' @importFrom stats setNames
#' @importFrom magrittr %<>%
#' @importFrom DSI datashield.connections_find datashield.aggregate
#' @importFrom utils capture.output
#'
#' @md
#'
#' @export
dh.getStats <- function(df = NULL, vars = NULL, digits = 2, conns = NULL,
                        checks = TRUE) { # nolint
  
  ################################################################################
  # 1. First checks
  ################################################################################
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  if (checks == TRUE) {
    .isDefined(df = df, conns = conns)
  }
  # Not checking whether variable exists because function will show NA if it
  # doesnt
  
  Mean <- perc_5 <- perc_25 <- perc_50 <- perc_75 <- perc_95 <- missing_perc <-
    variance <- variable <- category <- value <- cohort_n <- valid_n <-
    missing_n <- perc_missing <- EstimatedVar <- Nvalid <- any_obs <-
    bind_cols <- cohort <- combined <- disc <- discrepancy <- key_stats <-
    out_cont <- outcome <- same_levels <- se <- stat <-
    stats_tmp <- stats_wide <- std.dev <- type <- type_w_null <- . <-
    perc_valid <- perc_total <- Ntotal <- disclosure_fail <- NULL
  
  ################################################################################
  # 1. Remove duplicate variables  
  ################################################################################
  dups <- vars[duplicated(vars)]
  
  if(length(dups) >0){
    
    warning(paste0("The following variables specified in 'vars' are duplicated and will
            be removed: ", paste0(dups, collapse = ",")))
    
  }
  
  vars <- unique(vars)
  
  ################################################################################
  # 2. Get classes of variables
  ################################################################################
  check_class <- dh.classDiscrepancy(
    df = df,
    vars = vars,
    conns = conns,
    checks = FALSE
  )
  
  ################################################################################
  # 3. Check variable has the same class in each cohort
  ################################################################################
  
  ## We need to distinguish between variables which are NULL and variables which
  ## really have a different class.
  real_disc <- check_class %>%
    select(-variable, -discrepancy) %>%
    replace(. == "NULL", NA) %>%
    pmap_chr(function(...) {
      c(...) %>%
        n_distinct(na.rm = TRUE)
    }) %>%
    bind_cols(check_class, disc = .) %>%
    mutate(disc = ifelse(disc > 1, "yes", "no")) %>%
    dplyr::filter(disc == "yes")
  
  
  if (nrow(real_disc) > 0) {
    stop(
      "\nThe following variables specified in `vars` do not have the same class in all cohorts. Please
check with ds.class \n\n",
      real_disc %>% pull(variable) %>% paste(collapse = "\n"),
      call. = FALSE
    )
  }
  
  
  ################################################################################
  # 4. Check factor variables have the same levels in each cohort
  ################################################################################
  
  ## ---- Restrict to factors that exist in each cohort --------------------------
  fact_exist <- check_class %>%
    pivot_longer(
      cols = c(-variable, -discrepancy),
      values_to = "type",
      names_to = "cohort"
    ) %>%
    dplyr::filter(type == "factor")
  
  if (nrow(fact_exist) > 0) {
    ## ---- Get the levels of these factors ----------------------------------------
    check_levels <- fact_exist %>%
      group_by(variable) %>%
      group_split() %>%
      map(
        .,
        ~ pmap(., function(variable, cohort, ...) {
          cally <- paste0("levelsDS(", df, "$", variable, ")")
          datashield.aggregate(conns[cohort], as.symbol(cally))[[1]]$Levels
        })
      ) %>%
      set_names(sort(unique(fact_exist$variable)))
    
    ## ---- Check whether these levels are identical for all cohorts ---------------
    level_ref <- check_levels %>%
      map(function(x){x[!is.na(x)]}) %>%
      map(unique) %>%
      map(length) %>%
      bind_rows() %>%
      pivot_longer(
        cols = everything(),
        values_to = "length",
        names_to = "variable"
      ) %>%
      mutate(same_levels = ifelse(length == 1, "yes", "no")) %>%
      select(-length)
    
    if (any(level_ref$same_levels == "no") == TRUE) {
      stop(
        "The following categorical variables specified in `vars` do not have the same levels in all cohorts. Please check using ds.levels:\n\n",
        level_ref %>%
          dplyr::filter(same_levels == "no") %>%
          pull(variable) %>%
          paste(., collapse = "\n"),
        call. = FALSE
      )
    }
  }
  
  
  
  ################################################################################
  # 5. Get maximum ns for each cohort
  ################################################################################
  cohort_ns <- ds.dim(df, type = "split", datasources = conns) %>%
    map_df(~ .[1]) %>%
    set_names(names(conns)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "cohort",
      values_to = "cohort_n"
    )
  
  ################################################################################
  # 6. Identify variable classes
  ################################################################################
  
  ## ---- Table with classes of all variables ------------------------------------
  vars_long <- check_class %>%
    pivot_longer(
      cols = c(-variable, -discrepancy),
      names_to = "cohort",
      values_to = "type_w_null"
    ) %>%
    mutate(any_obs = ifelse(type_w_null == "NULL", "no", "yes")) %>%
    select(variable, cohort, any_obs, type_w_null)
  
  classes <- vars_long %>%
    distinct(variable, type_w_null) %>%
    dplyr::filter(type_w_null != "NULL") %>%
    dplyr::rename(type = type_w_null)
  
  vars_long <- left_join(vars_long, classes, by = "variable")
  
  ## ---- Final reference table for factors --------------------------------------
  fact_ref <- vars_long %>%
    dplyr::filter(type == "factor")
  
  if (nrow(fact_ref) > 0) {
    ## Here we get the possible levels of the factors
    unique_levels <- check_levels %>%
      map(unlist) %>%
      map(unique) %>%
      map(paste, collapse = ",") %>%
      as_tibble() %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "levels"
      )
    
    fact_ref <- left_join(fact_ref, unique_levels, by = "variable") %>%
      left_join(., cohort_ns, by = "cohort") %>%
      select(variable, cohort, any_obs, levels, cohort_n)
  }
  
  ## ---- Final reference table for continuous variables -------------------------
  cont_ref <- vars_long %>%
    dplyr::filter(type %in% c("numeric", "integer")) %>%
    select(variable, cohort, any_obs)
  
  ################################################################################
  # 7. Check for disclosure issues
  ################################################################################
  
  ## This is very inefficient at the moment, but I need to spend some time 
  ## thinking how to do this in a better way.
  
  if(nrow(fact_ref) > 0){
    
    pre_check <- fact_ref %>%
      pmap(function(variable, cohort, levels, ...){
        
        calltext <- call(
          "tableDS",
          rvar.transmit = paste0(df, "$", variable),
          cvar.transmit = NULL,
          stvar.transmit = NULL,
          rvar.all.unique.levels.transmit = levels,
          cvar.all.unique.levels.transmit = NULL,
          stvar.all.unique.levels.transmit = NULL,
          exclude.transmit = NULL,
          useNA.transmit = "always",
          force.nfilter.transmit = NULL
        )
        
        datashield.aggregate(conns[cohort], calltext) 
        
      })
    
    fact_ref <- fact_ref %>% mutate(
      disclosure_fail = pre_check %>% 
        map(~str_detect(.[[1]], "Failed")[[1]]) %>%
        unlist)
    
    invalid <- fact_ref %>% dplyr::filter(disclosure_fail == TRUE)
    
    fact_ref <- fact_ref %>% 
      dplyr::filter(disclosure_fail == FALSE) %>%
      dplyr::select(-disclosure_fail)
    
  }
  
  ################################################################################
  # 8. Extract statistics
  ################################################################################
  
  out_cat <- list()
  out_cont <- list()
  
  ## ---- Categorical ------------------------------------------------------------
  if (nrow(fact_ref) > 0) {
    stats_extracted <- .statsTable(ref = fact_ref, df = df, conns = conns) 
    
    stats_cat <- .tidyStats(fact_ref, stats_extracted) %>% 
      left_join(., cohort_ns, by = "cohort")
    
    ################################################################################
    # Check for invalid cases
    ################################################################################
    invalid <- stats_cat %>% dplyr::filter(str_detect(category, "Failed"))
    
    if(nrow(invalid) > 0){
      
      warning("These variables have insufficient cell count for at least
            one cohort and have been removed:")
      
      invalid %>% dplyr::select(variable, cohort) %>% print
      
    }
    
    stats_cat <- stats_cat %>% dplyr::filter(is.na(category) | !str_detect(category, "Failed"))
    
  }
  ## ---- Continuous -------------------------------------------------------------
  
  if (nrow(cont_ref) > 0) {
    
    quantiles_extracted <- .statsQuantMean(ref = cont_ref, df = df, conns = conns)
    variance_extracted <- .statsVar(ref = cont_ref, df = df, conns = conns)
    
    stats_cont <- bind_rows(
      quantiles = .tidyStats(cont_ref, quantiles_extracted),
      variance = .tidyStats(cont_ref, variance_extracted)
    )  
    
  }
  
  ################################################################################
  # 9. Calculate combined stats for categorical variables
  ################################################################################
  
  if (nrow(fact_ref) > 0) {
    
    ## ---- Combined value for each level of variables -----------------------------
    levels_comb <- stats_cat %>%
      group_by(variable, category) %>%
      summarise(
        value = sum(value, na.rm = TRUE)
      ) %>%
      mutate(cohort = "combined")
    
    ## ---- Combined n for each variable -------------------------------------------
    n_cat_comb <- stats_cat %>%
      group_by(variable) %>%
      distinct(cohort, .keep_all = TRUE) %>%
      summarise(
        cohort_n = sum(cohort_n, na.rm = TRUE)
      )
    
    
    ################################################################################
    # 10. Join back and calculate final categorical stats
    ################################################################################
    out_cat <- left_join(levels_comb, n_cat_comb, by = "variable") %>%
      bind_rows(., stats_cat)
    
    ## ---- Calculate valid n for each variable and cohort -------------------------
    cat_valid_n <- out_cat %>%
      group_by(cohort, variable) %>%
      dplyr::filter(!is.na(category)) %>%
      group_split() %>%
      map(~ mutate(., valid_n = sum(value, na.rm = TRUE))) %>%
      map(~ select(., variable, category, cohort, valid_n)) %>%
      bind_rows()
    
    ## ---- Final stats ------------------------------------------------------------
    out_cat <- left_join(
      out_cat, cat_valid_n,
      by = c("variable", "category", "cohort")) %>%
      mutate(
        missing_n = cohort_n - valid_n,
        perc_valid = (value / valid_n) * 100,
        perc_missing = (missing_n / cohort_n) * 100,
        perc_total = (value / cohort_n) * 100) %>%
      select(variable, cohort, category, value, everything()) %>%
      mutate(across(perc_valid:perc_total, ~ round(., digits))) %>%
      ungroup()
  }
  ################################################################################
  # 12. Calculate combined statistics for continuous stats
  ################################################################################
  
  key_stats <- c("Sum", "SumOfSquares", "Nmissing", "Nvalid", "Ntotal")
  
  if (nrow(cont_ref) > 0) {
    ## ---- Put key stats into wide format -----------------------------------------
    stats_wide <- stats_cont %>%
      dplyr::filter(stat %in% key_stats) %>%
      pivot_wider(
        values_from = value,
        names_from = stat
      )
    
    stats_cont_wide <- stats_cont %>%
      dplyr::filter(!stat %in% key_stats) %>%
      left_join(., stats_wide, by = c("variable", "cohort"))
    
    
    ## ---- Combined quantiles --------------------------------------------------------------
    quantiles_comb <- stats_cont_wide %>%
      dplyr::filter(!stat == "EstimatedVar") %>%
      group_by(variable, stat) %>%
      group_split() %>%
      map(
        ~ mutate(.,
                 weight = Nvalid / sum(Nvalid, na.rm = TRUE),
                 weighted_val = value * weight,
                 combined = sum(weighted_val, na.rm = TRUE)
        )
      ) %>%
      map(
        ~ select(., variable, stat, combined)
      ) %>%
      map(
        ~ slice(., 1)
      ) %>%
      bind_rows() %>%
      rename(value = combined) %>%
      mutate(cohort = "combined")
    
    
    ## ---- Combined variance ---------------------------------------------------------------
    var_comb <- stats_cont_wide %>%
      dplyr::filter(stat == "EstimatedVar") %>%
      group_by(variable, stat) %>%
      group_split() %>%
      map(
        ~ summarise(.,
                    GlobalSum = sum(Sum, na.rm = TRUE),
                    GlobalSumSquares = sum(SumOfSquares, na.rm = TRUE),
                    GlobalNvalid = sum(Nvalid, na.rm = TRUE),
                    EstimatedVar =
                      GlobalSumSquares / (GlobalNvalid - 1) -
                      (GlobalSum^2) / (GlobalNvalid * (GlobalNvalid - 1)),
                    Nvalid = GlobalNvalid,
                    Ntotal = sum(Ntotal, na.rm = TRUE)
        )
      ) %>%
      map(~ select(., EstimatedVar, Nvalid, Ntotal)) %>%
      set_names(sort(unique(cont_ref$variable))) %>%
      bind_rows(.id = "variable") %>%
      mutate(cohort = "combined") %>%
      pivot_longer(
        cols = c(EstimatedVar, Nvalid, Ntotal),
        values_to = "value",
        names_to = "stat"
      )
    
    ################################################################################
    # 13. Join back and calculate final continuous stats
    ################################################################################
    out_cont <- bind_rows(list(stats_cont, quantiles_comb, var_comb)) %>%
      pivot_wider(
        names_from = "stat",
        values_from = "value"
      ) %>%
      left_join(., cohort_ns, by = "cohort") %>%
      mutate(
        std.dev = sqrt(EstimatedVar),
        valid_n = replace_na(Nvalid, 0),
        cohort_n = Ntotal,
        missing_n = cohort_n - valid_n,
        missing_perc = (missing_n / cohort_n) * 100
      ) %>%
      select(
        variable, cohort, mean, std.dev, perc_5:perc_95,
        valid_n, cohort_n, missing_n, missing_perc
      ) %>%
      mutate(across(mean:missing_perc, ~ round(., digits))) %>%
      ungroup()
  }
  ################################################################################
  # 14. Join categorical and continuous as output
  ################################################################################
  out <- list(
    categorical = out_cat,
    continuous = out_cont
  )
  
  if(nrow(fact_ref) > 0){
    
    if(nrow(invalid) > 0){
      
      invalid_out <- invalid %>% dplyr::select(variable, cohort)
      
      warning(paste(capture.output({
        cat("These variables have insufficient cell count for at least
          one cohort and have been removed:\n\n")
        print(invalid_out)
      }), collapse = "\n"))
      
    }
    
  }
  
  return(out)
}

#' Extracts stats using table function
#' 
#' @param ref reference tibble of vars with four columns: variable, cohort, 
#' any_obs and levels.
#' 
#' @importFrom dplyr %>% group_by bind_rows group_map
#' @importFrom purrr set_names pmap
#' @importFrom tibble tibble as_tibble
#' @importFrom DSI datashield.aggregate
#' 
#' @noRd
.statsTable <- function(ref, df, conns){
  
  variable <- NULL
  
  ref %>%
    group_by(variable) %>%
    group_map(
      ~ pmap(., function(cohort, any_obs, levels, cohort_n) {
        
        if (any_obs == "no") {
          tibble(
            category = c(unlist(strsplit(levels, ",")), NA),
            value = c(rep(0, length(category) - 1), cohort_n)
          )
        } else {
          calltext <- call(
            "tableDS",
            rvar.transmit = paste0(df, "$", .y),
            cvar.transmit = NULL,
            stvar.transmit = NULL,
            rvar.all.unique.levels.transmit = levels,
            cvar.all.unique.levels.transmit = NULL,
            stvar.all.unique.levels.transmit = NULL,
            exclude.transmit = NULL,
            useNA.transmit = "always",
            force.nfilter.transmit = NULL
          )
          
          datashield.aggregate(conns[cohort], calltext) %>%
            as.data.frame() %>%
            as_tibble() %>%
            set_names(c("category", "value"))
        }
      })
    )
}

#' Extracts stats using quantile mean
#' 
#' @param ref reference tibble of vars with four columns: variable, cohort, 
#' any_obs and levels.
#' 
#' @importFrom dplyr %>% group_by mutate case_when bind_rows group_map
#' @importFrom tidyr pivot_longer
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @importFrom DSI datashield.aggregate
#' 
#' @noRd      
.statsQuantMean <- function(ref, df, conns){
  
  variable <- NULL
  
  ref %>%
    group_by(variable) %>%
    group_map(
      ~ pmap(., function(cohort, any_obs, levels, cohort_n) {
        
        if (any_obs == "no") {
          tibble(
            stat = c(
              "perc_5", "perc_10", "perc_25", "perc_50", "perc_75", "perc_90",
              "perc_95", "mean"
            ),
            value = NA
          )
        } else {
          calltext <- paste0("quantileMeanDS(", df, "$", .y, ")")
          datashield.aggregate(conns[cohort], calltext) %>%
            bind_rows() %>%
            pivot_longer(
              cols = everything(),
              names_to = "stat",
              values_to = "value"
            ) %>%
            mutate(
              stat = case_when(
                stat == "5%" ~ "perc_5",
                stat == "10%" ~ "perc_10",
                stat == "25%" ~ "perc_25",
                stat == "50%" ~ "perc_50",
                stat == "75%" ~ "perc_75",
                stat == "90%" ~ "perc_90",
                stat == "95%" ~ "perc_95",
                stat == "Mean" ~ "mean"
              )
            )
        }
        
      })
    )
}

#' Extracts stats using variance function
#' 
#' @param ref reference tibble of vars with four columns: variable, cohort, 
#' any_obs and levels.
#' 
#' @importFrom dplyr %>% group_by mutate case_when select bind_rows group_map
#' @importFrom tidyr pivot_longer
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @importFrom DSI datashield.aggregate
#' 
#' @noRd     
.statsVar <- function(ref, df, conns){
  
  variable <- NULL
  
  ref %>%
    group_by(variable) %>%
    group_map(
      ~ pmap(., function(cohort, any_obs, levels, cohort_n) {
        
        if (any_obs == "no") {
          tibble(
            stat = c(
              "SumOfSquares", "Nmissing", "Nvalid", "Ntotal",
              "EstimatedVar"
            ),
            value = NA
          )
        } else {
          calltext <- paste0("varDS(", df, "$", .y, ")")
          datashield.aggregate(conns[cohort], calltext)[[1]] %>%
            bind_rows() %>%
            mutate(EstimatedVar = SumOfSquares / (Nvalid - 1) - (Sum^2 / (Nvalid * (Nvalid - 1)))) %>%
            dplyr::select(-ValidityMessage) %>%
            pivot_longer(
              cols = c(Sum, SumOfSquares, Nmissing, Nvalid, Ntotal, EstimatedVar),
              names_to = "stat",
              values_to = "value"
            )
        }
      })
      
    )
  
}

#' Tidies the output of .statsTable, .statsQuantMean or .statsVar
#' 
#' @param ref reference tibble of vars with four columns: variable, cohort, 
#' any_obs and levels.
#' @param stats output of one of the aforementioned functions.
#' 
#' @importFrom dplyr %>% group_by group_split bind_rows
#' @importFrom purrr map set_names
#' @importFrom tibble tibble
#' @importFrom DSI datashield.aggregate
#' 
#' @noRd   
.tidyStats <- function(ref, stats){
  
  variable <- NULL
  
  cohort_names <- ref %>%
    group_by(variable) %>%
    group_split %>%
    map(~.$cohort)
  
  out <- list(stats, cohort_names) %>%
    pmap(function(stats, cohort_names){
      
      set_names(stats, cohort_names)
      
    }) %>% 
    set_names(sort(unique(ref$variable))) %>%
    map(bind_rows, .id = "cohort") %>%
    bind_rows(.id = "variable")
  
  return(out)
  
}

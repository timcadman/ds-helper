#' Creates tables in useful formats for including in manuscripts
#'
#' dh.getStats extracts key statistics and stores them in a clientside list. 
#' dh.createTableOne builds on this by formatting the extract stats into a 
#' table which can then be included in a manuscript. Flexible formatting options
#' are included. 
#'
#' @param stats Exported object from dh.getStats.
#' @param vars Variable to be included in table.
#' @param var_labs Tibble with two columns: 'variable' containing the 
#' names of the variables specified in `vars`, and 'var_label' containing the
#' replacement labels for these variables.
#' @param cat_labs Tibble with three columns: 'variable' containing the 
#' names of the categorical variables specified in `vars`, 'category' 
#' containing the categories of these variabels, and "cat_label" containing
#' the replacement category labels for these variables.
#' @param coh_labs Tibble with two columns: 'cohort' containing the names
#' of all cohorts included in `stats`, and 'cohort_labs' containing the 
#' replacement labels for these cohorts.
#' @param type Character specifying which cohorts to include in the table. If
#' "combined" then only combined stats will be returned, if "cohort" then only
#' cohort-specific stats will be returned, if "both" then everything will be 
#' returned.
#' @param coh_direction Character specifying direction of data if `type` is 
#' 'cohort' or 'both'. Use 'rows' to return cohorts as rows and variable as 
#' columns, or use 'cols' to return cohorts as columns and variables as rows.
#' Defauls is "col".
#' @param cont_format Character specifying which summary statistic to return
#' for continuous stats. Use 'med_iqr' to return the median and interquartile
#' range, use 'mean_sd' to return the mean and standard deviation. Default is
#' "med_iqr".
#' @param inc_missing Boolean specifying whether to return missing values in
#' the output. Use TRUE for yes and FALSE for no.
#' @param round_digits Optionally, the number of decimal places to round output 
#' to. Default is 2.
#' @param perc_denom The denominator for percentages. Either 'valid' for valid
#' cases or 'total' for total cases.
#'
#' @return Tibble containing formatted summary statistics. If `coh_direction` is
#' 'cols', the tibble will contain four columns: 'cohort', 'variable', 
#' 'category' & value. If `coh_direction` is rows, the tibble will contain the
#' column 'cohort' as well as as columns for all continuous variables and all
#' categories of categorical variables.    
#'
#' @family descriptive functions
#'
#' @importFrom dplyr %>% left_join mutate select filter bind_rows
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang arg_match is_bool
#'
#' @export
dh.createTableOne <- function(stats = NULL, vars = NULL, var_labs = NULL, 
                              cat_labs = NULL, type = NULL, coh_labs = NULL, 
                              coh_direction = "cols", cont_format = "med_iqr", 
                              inc_missing = NULL, round_digits = 2, 
                              perc_denom = NULL){
  
  variable <- . <- cat_label <- var_label <- cohort <- value <- data_type <-
    miss_n_perc <- category <- coh_label <- avail_stats <- vars_list <- 
    stats_cat <- stats_cont <- old_var <- cohort_labs <- stats_sub_coh <- 
    stats_sub_vars <- NULL
  
  .checkArgs()
  
  .checkVarsInStats()
  
  stats_sub_vars <- .subsetVars()
  
  stats_sub_coh <- .subsetCoh()
  
  if(nrow(stats$categorical) > 0) {
    
    stats_cat <- .formatCatStats()
    
    if(!is.null(cat_labs)){
      
      .checkLabsMatchCats()
      cat_labs <- .cleanLabs(cat_labs, c("variable", "category", "cat_label"))
      
      stats_cat <- stats_cat %>%
        left_join(., cat_labs, by = c("variable", "category")) %>%
        dplyr::select(cohort, variable, category = cat_label, value)
      
    }
    
  }
  
  if(nrow(stats$continuous) > 0) {
    
    stats_cont <- .formatContStats()
    
  }
  
  out <- bind_rows(stats_cat, stats_cont) %>%
    dplyr::mutate(variable = factor(variable, levels = vars)) %>%
    arrange(variable)
  
  if(!is.null(var_labs)){
    
    var_labs <- .cleanLabs(var_labs, c("variable", "var_label"))
    
    out <- out %>%
      left_join(., var_labs, by = "variable") %>%
      dplyr::select(cohort, variable = var_label, category, value)

    
  }
  
  if(!is.null(coh_labs)){
    
    coh_labs <- .cleanLabs(coh_labs, c("cohort", "cohort_labs"))
    
    out <- left_join(out, coh_labs, by = "cohort") %>%
      dplyr::select(cohort = cohort_labs, variable, category, value) 
    
  }
  
  if(inc_missing == FALSE){
    
    out <- out %>%
      dplyr::filter(!is.na(category))
    
  } 
  
  if(coh_direction == "cols"){
    
    out <- out %>%
      pivot_wider(
        names_from = cohort,
        values_from = value)
      
  } else if(coh_direction == "rows"){
      
    out <- out %>%
      pivot_wider(
        names_from = c(variable, category),
        values_from = value) 
    
    }

  return(out)
  
}

#' Performs argument checks
#' 
#' @return Nothing if checks pass, else throws an error
#' 
#' @importFrom checkmate assert_list assert_character assert_choice 
#' assert_logical assert_data_frame assert_subset
#' 
#' @noRd
.checkArgs <- function(stats, type, inc_missing, perc_denom, cat_labs, var_labs,
                       coh_labs, coh_direction, cont_format){
  
  assert_list(stats)
  assert_character(vars)
  assert_choice(type, c("cohort", "combined", "both"))
  assert_choice(coh_direction, c("rows", "cols"))
  assert_choice(cont_format, c("med_iqr", "mean_sd"))
  assert_logical(inc_missing)
  assert_choice(perc_denom, c("valid", "total"))
  
  if(!is.null(cat_labs)){
    
    assert_data_frame(cat_labs)
    assert_subset(c("variable", "category", "cat_label"), colnames(cat_labs))
    
  }
  
  if(!is.null(var_labs)){
  
    assert_data_frame(var_labs)
    assert_subset(c("variable", "var_label"), colnames(var_labs))
    assert_subset(vars, var_labs$variable)

  }
  
  if(!is.null(coh_labs)){
    
    assert_data_frame(coh_labs)
    assert_subset(c("cohort", "cohort_labs"), colnames(coh_labs))
    
    distinct_cohorts <- map(stats, ~.x$cohort) %>% 
      unlist %>%
      unique
    
    assert_subset(distinct_cohorts, coh_labs$cohort)
    
  }
  
}
  
#' Checks that all the variable names provided to `vars` are available in the
#' object provided to `stats`
#' 
#' 
#' @return Nothing if all variable provided in `vars` exist in `stats`, else
#' throws an error.
#' 
#' @noRd
.checkVarsInStats <- function(stats, vars){
  
  stats_vars <- stats %>%
    map(~.x$variable) %>%
    unlist %>%
    unique
  
  avail <- vars[vars %in% stats_vars]
  not_avail <- vars[!vars %in% stats_vars]
  
  if(length(not_avail > 0)){
    
    stop(
      paste0(
        "The following variables provided in `vars` are not present in the
      statistics provided in `stats`\n\n", 
        paste0(not_avail, collapse = ", "))
    )
    
  }
  
}

#' Subsets `stats` to include only variables specified in `vars`
#' 
#' @return Subset of `stats`
#' 
#' @noRd
.subsetVars <- function(stats){
  
  stats %>%
    map(~dplyr::filter(., variable %in% vars))
  
}

#' Subset stats based on argument to `type`
#' 
#' @noRd
.subsetCoh <- function(type, stats_sub_vars){
  
  if(type == "combined"){
    
    out <- stats_sub_vars %>%
    map(~dplyr::filter(., cohort == "combined"))
    
    } else if(type == "cohort"){
  
      out <- stats_sub_vars %>%
    map(~dplyr::filter(., cohort != "combined"))
  
    } else if(type == "both"){
    
      out <- stats_sub_vars
      
    }
  
}
  
#' Performs the initial formatting of categorical statistics
#' 
#' 
#' @return Tibble containing 5 columns: 'cohort', 'variable', 'category', 
#' 'value' and 'data_type'
#' 
#' @importFrom dplyr %>% filter mutate select
#' 
#' @noRd
#' 
.formatCatStats <- function(stats_sub_coh, perc_denom){
  
  perc_valid <- perc_total <- value <- category <- cohort <- variable <- NULL
  
  out <- stats_sub_coh$categorical %>%
    mutate(
      across(c(perc_valid, perc_total), ~signif(., round_digits))) 
 
  if(perc_denom == "total"){
    
    out <- out %>%
      mutate(value = paste0(value, " (", perc_total, ")")) 
    
  } else if(perc_denom == "valid"){
    
    out <- out %>%
      mutate(value = ifelse(
        !is.na(category), 
        paste0(value, " (", perc_valid, ")") ,
        paste0(value, " (", perc_total, ")"))) 
    
  }
  
  out <- out  %>% 
    dplyr::select(cohort, variable, category, value)
  
  return(out)
  
}

#' Performs the initial formatting of continuous statistics
#' 
#' @param stats Exported object from dh.getStats
#' @param vars Character vector of variable names
#' @param cont_stats Character specifying which summary statistic to return
#' for continuous stats. Use 'med_iqr' to return the median and interquartile
#' range, use 'mean_sd' to return the mean and standard deviation.
#' @param inc_missing Boolean specifying whether to return missing values in
#' the output. Use TRUE for yes and FALSE for no.
#' 
#' @return Tibble containing 5 columns: 'cohort', 'variable', 'category', 
#' 'value' and 'data_type'. If `inc_missing` is TRUE contains an sixth column
#' 'miss_n_perc'
#' 
#' @importFrom dplyr %>% filter mutate select
#' 
#' @noRd
.formatContStats <- function(stats_sub_coh, cont_format){
  
  perc_95 <- missing_perc <- valid_n <- missing_n <- category <- std.dev <-
    perc_50 <- perc_25 <- perc_75 <- value <- cohort <- variable <- NULL
  
  out <- stats_sub_coh$continuous %>%
    mutate(
      across(
        c(mean:perc_95, missing_perc), ~signif(., round_digits))) 
  
  out <- out %>%
    pivot_longer(
      cols = c(valid_n, missing_n),
      names_to = "category", 
      values_to = "missing") %>%
    mutate(category = ifelse(category == "missing_n", NA, cont_format))
  
  if(cont_format == "mean_sd"){
    
    out <- out %>%
      mutate(value = paste0(mean, " \u00b1 ", std.dev)) 
    
  } else if(cont_format == "med_iqr"){
    
    out <- out %>%
      mutate(value = paste0(perc_50, " (", perc_25, ",", perc_75, ")")) 
    
  }
  
  out <- out %>%
    mutate(value = ifelse(is.na(category), 
                          paste0(missing, " (", missing_perc, ")"), value)) %>%
    dplyr::select(cohort, variable, category, value)
  
  return(out)
  
}

#' Checks that all categorical variables provided to `stats_cat` have 
#' corresponding labels provided in `cat_labs`
#' 
#' 
#' @return Returns an error if labels have not been provided for all levels of
#' all categorical variables. 
#' 
#' @importFrom dplyr left_join %>% filter 
#' 
#' @noRd
.checkLabsMatchCats <- function(stats_cat, cat_labs, category, cat_label){
  
  test_cats <- left_join(stats_cat, cat_labs, by = c("variable", "category")) %>%
    dplyr::filter(category != "missing")
  
  missing_cats <- test_cats %>% 
    dplyr::filter(!is.na(category) & is.na(cat_label))
  
  if(length(missing_cats > 0)){
    
    stop(
      "The following categorical variables are included in 'vars' 
      but do not have a corresponding labels for all categories provided in 
      `cat_labs`\n\n")
    
    print(missing_cats)
    
  }
  
}

#' Removes duplicates and selects only required columns
#' 
#' @return Cleaned dataframe of labels
#' 
#' @noRd
.cleanLabs <- function(labs, cols){
  
out <- labs %>%
  distinct %>%
  dplyr::select(all_of(cols))

return(out)

}

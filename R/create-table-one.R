dh.createTableOne(
  stats = descriptives_all, 
  vars = vars_ordered, 
  var_labs = full.ref, 
  cat_labs = sample_cat.ref,
  type = "combined", 
  inc_missing = TRUE, 
  round_digits = 3, 
  perc_denom = "valid")) 

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
#' @param cont_stats Character specifying which summary statistic to return
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
                              coh_direction = "cols", cont_stats = "med_iqr", 
                              inc_missing = NULL, round_digits = 2, 
                              perc_denom = NULL){
  
  variable <- . <- cat_label <- var_label <- cohort <- value <- data_type <-
    miss_n_perc <- category <- coh_label <- avail_stats <- vars_list <- 
    stats_cat <- stats_cont <- old_var <- NULL
  
  previous 650 lines
  
  .checks()
  .checkVarsInStats()
  stats_sub <- .subsetVars()
  
  if(length(stats$categorical) > 0) {
    
    stats_cat <- .formatCatStats()
    
  }
  
  if(length(stats$continuous) > 0) {
    
    stats_cont <- .formatContStats()
    
  }
 
  if(!is.null(cat_labs)){
    
    .checkLabsMatchCats()
    cat_labs <- .cleanLabs(cat_labs, c("variable", "category", "cat_label"))
    
    stats_cat <- stats_cat %>%
      left_join(., cat_labs, by = c("variable", "category")) %>%
      dplyr::select(cohort, variable, category = cat_label, value)
    
  }
  
  out <- bind_rows(stats_cat, stats_cont)
  
  if(!is.null(var_labs)){
    
    var_labs <- .cleanLabs(var_labs, c("variable", "var_label"))
    
    out <- out %>%
      left_join(., var_labs, by = "variable") %>%
      dplyr::select(cohort, variable = var_label, category, value)
      
      mutate(old_var = variable) %>%
      mutate(variable = var_label) %>%
      dplyr::select(-var_label)
    
  }
  
  if(!is.null(coh_labs)){
    
    .checkLabsMatchCoh(stats, coh_labs, stats_types)
    
    out <- left_join(out, coh_labs, by = "cohort") %>%
      dplyr::select(-cohort) %>%
      dplyr::rename(cohort = coh_label) %>%
      dplyr::select(cohort, everything())
    
  }
  
  
  if(inc_missing == FALSE){
    
    out <- out %>%
      dplyr::filter(!is.na(category))
    
  } 
  

  


  
  
  out$category

  mutate(perc_valid = signif(perc_valid, round_digits))

  

  

  
  #stats_types <- .checkAvailStats(stats)
  
  #avail_stats <- .splitStatsVars(stats, stats_types)
  
  #vars_list <- .splitTargetVars(avail_stats, vars)
  
  
  
 
  

  

  
  %>%
    mutate(variable = factor(variable, levels = vars, ordered = T)) %>%
    arrange(variable)
  

  

  

  
  if(type == "combined"){
    
    out <- out %>%
      dplyr::filter(cohort == "combined") %>%
      dplyr::select(-data_type, -old_var)
    
  } else if(type == "cohort"){
    
    out <- out %>%
      dplyr::filter(cohort != "combined")
    
  }
    
    if(type %in% c("cohort", "both")){
      
      if(coh_direction == "cols"){

        if(inc_missing == FALSE){
          
          out <- out %>%
            pivot_wider(
              names_from = cohort,
              values_from = value)
          
        } else if(inc_missing == TRUE){
          
          out_cat <- out %>%
            dplyr::filter(data_type == "cat") %>%
            dplyr::select(-miss_n_perc) %>%
            mutate(category = ifelse(is.na(category), "miss_n_perc", category)) %>%
            pivot_wider(
              names_from = cohort,
              values_from = value)
          
          out_cont <- out %>%
            dplyr::filter(data_type == "cont") %>%
             pivot_wider(
              names_from = cohort,
              values_from = value)
          
          out <- bind_rows(out_cat, out_cont) 
          
        }
        
        out <- out %>%
          dplyr::select(-data_type)
        
        } else if(coh_direction == "rows"){
        
        if(inc_missing == FALSE){
          
    out <- out %>%
      dplyr::select(-data_type) %>%
      pivot_wider(
        names_from = c(variable, category),
        values_from = value) 
    
        } else if(inc_missing == TRUE){
          
          out_cat <- out %>%
            dplyr::filter(data_type == "cat") %>%
            mutate(category = ifelse(is.na(category), "miss_n_perc", category)) %>%
            dplyr::select(-miss_n_perc) 
          
          out_cont <- out %>%
            dplyr::filter(data_type == "cont") %>%
            dplyr::select(-category) %>%
            pivot_longer(
              cols = c(value, miss_n_perc), 
              names_to = "category", 
              values_to = "value")
          
          out <- bind_rows(out_cat, out_cont) %>%
            dplyr::select(-data_type) %>%
            mutate(old_var = factor(old_var, levels = vars, ordered = TRUE)) %>%
            arrange(old_var)
          
        }
          
        }
    
    }
      
  avail_coh <- .availCoh(stats, stats_types)
  
  avail_coh <- c(avail_coh[!avail_coh == "combined"], "combined")
  
  if(coh_direction == "rows"){
    
  out <- out %>%
    mutate(cohort = factor(cohort, levels = avail_coh, ordered = TRUE))
  
  } 
  
 # out <- out %>%
#    dplyr::select(-old_var)
  
  return(out)
  
}


#' Performs argument checks
#' 
#' @return Nothing if checks pass, else throws an error
#' 
#' @noRd
.checks <- function(){
  
  assert_list(stats)
  assert_character(vars)
  assert_choice(type, c("cohort", "combined", "both"))
  assert_choice(coh_direction, c("rows", "cols"))
  assert_choice(cont_stats, c("med_iqr", "mean_sd"))
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
.checkVarsInStats <- function(){
  
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
.subsetVars <- function(){
  
  stats %>%
    map(~dplyr::filter(., variable %in% vars))
  
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
.formatCatStats <- function(){
  
  if(perc_denom == "total"){
    
    out <- stats_sub$categorical %>%
      mutate(value = paste0(value, " (", perc_total, ")")) 
    
  } else if(perc_denom == "valid"){
    
    out <- stats_sub$categorica %>%
      mutate(value = ifelse(
        !is.na(category), 
        paste0(value, " (", perc_valid, ")") ,
        paste0(value, " (", perc_total, ")"))) 
    
  }
  
  out <- out  %>% 
    dplyr::select(cohort, variable, category, value) %>%
    mutate(
      category = ifelse(is.na(category), "missing", as.character(category)))
  
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
.formatContStats <- function(){
  
  out <- stats_sub$continuous %>%
    pivot_longer(
      cols = c(valid_n, missing_n),
      names_to = "category", 
      values_to = "missing") 
  
  if(cont_stats == "mean_sd"){
    
    out <- out %>%
      mutate(value = paste0(mean, " \u00b1 ", std.dev)) 
    
  } else if(cont_stats == "med_iqr"){
    
    out <- out %>%
      mutate(value = paste0(perc_50, " (", perc_25, ",", perc_75, ")")) 
    
  }
  
  out <- out %>%
    mutate(value = ifelse(category == "missing_n", 
                          paste0(missing, " (", missing_perc, ")"), value)) %>%
    dplyr::select(cohort, variable, category, value) %>%
    mutate(category = case_when(
      category == "valid_n" ~ "value",
      category == "missing_n" ~ "missing"))
  
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
.checkLabsMatchCats <- function(){
  
  category <- cat_label <- NULL
  
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


#' Identifies whether there are both categorical and continuous stats
#' 
#' @param Exported object from dh.getStats
#' 
#' @return Character vector containing the names of available stats objects
#' 
#' @noRd
.checkAvailStats <- function(stats){

  var_types <- NULL

if(length(stats$categorical) > 0 & length(stats$continuous) > 0 ){

  var_types <- c("categorical", "continuous")  

} else if (length(stats$categorical) > 0 & length(stats$continuous) == 0 ){
  
  var_types <- "categorical"
  
} else if (length(stats$categorical) == 0 & length(stats$continuous) > 0 ){
  
  var_types <- "continuous"
  
}
  
}


#' Extracts variables names from `stats` object.
#' 
#' @param Exported object from dh.getStats
#' 
#' @return List with two elements named 'categorical' and 'continuous' 
#' containing character vectors of respective variable names.
#' 
#' @importFrom dplyr %>% pull
#' @importFrom purrr map set_names
#' 
#' @noRd
.splitStatsVars <- function(stats, var_types){
  
  variable <- NULL
  

  
  out <- var_types  %>%
    map(function(x){
      
      stats[[x]] %>% pull(variable) %>% unique
      
    }) %>%
    set_names(var_types)
  
  return(out)
  
}


#' Divides variable names provided to `vars` into categorical and continuous 
#' using the variable types in `stats`
#' 
#' @param stats_vars Output from .splitStatsVars
#' @param target_vars Vector of variable names provided to `vars`
#' 
#' @return List with two elements named 'categorical' and 'continuous' 
#' containing character vectors of respective variable names.
#' 
#' @importFrom dplyr %>%
#' @importFrom purrr map set_names
#' 
#' @noRd
.splitTargetVars <- function(stats_vars, target_vars){
                             
var_types <- c("categorical", "continuous")
  
  out <- var_types %>%
    map(function(x){
      
      stats_vars[[x]][stats_vars[[x]] %in% target_vars]    
      
    }) %>%
    set_names(var_types)
  
  return(out)
  
}



#' Makes character vector of cohorts present within `stats`
#' 
#' @param stats Exported object from dh.getStats
#' 
#' @return Character vector of cohort names
#' 
#' @importFrom dplyr %>% pull
#' 
#' @noRd
.availCoh <- function(stats, stats_types){
  
  cohort <- NULL 
  
out <- stats_types %>%
  map(function(x){stats[[x]] %>% pull(cohort) %>% unique}) %>%
  unlist %>%
  unique

return(out)

}



#' Creates tables in useful formats for including in manuscripts
#'
#' dh.getStats extracts key statistics and stores them in a clientside list. 
#' dh.createTableOne builds on this by formatting the extract stats into a 
#' table which can then be included in a manuscript. Flexible formatting options
#' are included. 
#'
#' @param stats Exported object from dh.getStats.
#' @template vars
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
#'
#' @return Tibble containing formatted summary statistics. If `coh_direction` is
#' 'cols', the tibble will contain four columns: 'cohort', 'variable', 
#' 'category' & value. If `coh_direction` is rows, the tibble will contain the
#' column 'cohort' as well as as columns for all continuous variables and all
#' categories of categorical variables.    
#'
#' @family descriptive functions
#'
#' @importFrom dplyr %>% left_join mutate select filter pivot_wider pivot_longer
#' bind_rows
#' @importFrom rlang arg_match is_bool
#'
#' @export
dh.createTableOne <- function(stats = NULL, vars = NULL, var_labs = NULL, 
                              cat_labs = NULL, type = NULL, coh_labs = NULL, 
                              coh_direction = "col", cont_stats = "med_iqr", 
                              inc_missing = NULL, round_digits = 2){
  
  variable <- . <- cat_label <- var_label <- cohort <- value <- data_type <-
    miss_n_perc <- category <- coh_label <- NULL
  
  if (is.null(stats)) {
    stop("`stats` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(vars)) {
    stop("`vars` must not be NULL.", call. = FALSE)
  }

  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  } 
  
  type <- arg_match(type, c("cohort", "combined", "both"))

  if (is.null(coh_direction)) {
    stop("`coh_direction` must not be NULL.", call. = FALSE)
  } 
  
  coh_direction <- arg_match(coh_direction, c("row", "col"))

  if (is.null(cont_stats)) {
    stop("`cont_stats` must not be NULL.", call. = FALSE)
  } 
  
  cont_stats <- arg_match(cont_stats, c("med_iqr", "mean_sd"))
  
  if (is.null(inc_missing)) {
    stop("`inc_missing` must not be NULL.", call. = FALSE)
  } 

  
  if (is_bool(inc_missing) == FALSE){
    stop("`inc_missing` must be either TRUE or FALSE")
  }
  
  avail_stats <- .splitStatsVars(stats)
  
  .checkVarsAvail(unlist(avail_stats), vars)
  
  vars_list <- .splitTargetVars(avail_stats, vars)
  
  stats_cat <- .formatCatStats(stats, vars_list$categorical, inc_missing, 
                               round_digits)
  stats_cont <- .formatContStats(stats, vars_list$continuous, cont_stats, 
                                 inc_missing, round_digits)
  
  out <-  bind_rows(stats_cat, stats_cont) %>%
    mutate(variable = factor(variable, levels = vars)) %>%
    arrange(variable)
  
  if(!is.null(cat_labs)){
    
    .checkLabType(cat_labs)
    .checkLabCols(cat_labs, 3, c("variable", "category", "cat_label"))
    .checkLabsMatchCats(stats_cat, cat_labs)
    
    out <- out %>%
      left_join(., cat_labs, by = c("variable", "category")) %>%
      mutate(category = cat_label) %>%
      dplyr::select(-cat_label)
    
  }
  
  if(!is.null(var_labs)){
    
    .checkLabType(var_labs)
    .checkLabCols(var_labs, 2, c("variable", "var_label"))
    .checkLabsMatchVars(vars, var_labs)
    
    out <- out %>%
      left_join(., var_labs, by = "variable") %>%
      mutate(variable = var_label) %>%
      dplyr::select(-var_label)
    
  }
  
  if(type == "combined"){
    
    out <- out %>%
      dplyr::filter(cohort == "combined")
    
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
            dplyr::select(-category) %>%
            pivot_longer(
              cols = c(value, miss_n_perc), 
              names_to = "category", 
              values_to = "value") %>%
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
            pivot_wider(
              names_from = c(variable, category), 
              values_from = value)
          
        }
          
        }
    
    }
      
  avail_coh <- .availCoh(stats)
  
  avail_coh <- c(avail_coh[!avail_coh == "combined"], "combined")
  
  out <- out %>%
    mutate(cohort = factor(cohort, levels = avail_coh, ordered = TRUE))
  
    if(!is.null(coh_labs)){
  
    .checkLabType(coh_labs)
    .checkLabCols(coh_labs, 2, c("cohort", "coh_label"))
    .checkLabsMatchCoh(stats, coh_labs)
      
    out <- left_join(out, coh_labs, by = "cohort") %>%
      dplyr::select(-cohort) %>%
      dplyr::rename(cohort = coh_label) %>%
      dplyr::select(cohort, everything())
    
    }
  
  return(out)
  
}

#' Extracts variables names from `stats` object.
#' 
#' @param Exported object from dh.getStats
#' 
#' @return List with two elements named 'categorical' and 'continuous' 
#' containing character vectors of respective variable names.
#' 
#' @importFrom dplyr %>% pull unique
#' @imporFrom purrr map set_names
#' 
#' @noRd
.splitStatsVars <- function(stats){
  
  variable <- NULL
  
  var_types <- c("categorical", "continuous")
  
  out <- var_types  %>%
    map(function(x){
      
      stats[[x]] %>% pull(variable) %>% unique
      
    }) %>%
    set_names(var_types)
  
  return(out)
  
}

#' Checks that all the variable names provided to `vars` are available in the
#' object provided to `stats`
#' 
#' @param source_vars Vector of variables names extracted from `stats`
#' @param target_vars Vector of variable names provided to `vars`
#' 
#' @return Returns an error if all variables in `target_vars` are not included
#' in `source_vars`, otherwise nothing is returned.
#' 
#' @noRd
.checkVarsAvail <- function(source_vars, target_vars){
  
  avail <- vars[vars %in% source_vars]
  not_avail <- vars[!vars %in% source_vars]
  
  if(length(not_avail > 0)){
    
    stop(
      paste0(
        "The following variables provided in `vars` are not present in the
      statistics provided in `stats`\n\n", 
        paste0(not_avail, collapse = ", "))
    )
    
  }
  
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

#' Performs the initial formatting of categorical statistics
#' 
#' @param stats Exported object from dh.getStats
#' @param vars Character vector of variable names
#' @inc_missing Boolean specifying whether to return missing values in
#' the output. Use TRUE for yes and FALSE for no.
#' 
#' @return Tibble containing 5 columns: 'cohort', 'variable', 'category', 
#' 'value' and 'data_type'
#' 
#' @importFrom dplyr %>% filter mutate select
#' 
#' @noRd
.formatCatStats <- function(stats, vars, inc_missing, round_digits){
  
  variable <- value <- perc_valid <- cohort <- category <- NULL
  
  out <- stats$categorical %>%
    dplyr::filter(variable %in% vars) %>%
    mutate(perc_valid = round(perc_valid, round_digits)) %>%
    mutate(value = paste0(value, " (", perc_valid, ")")) %>% 
    dplyr::select(cohort, variable, category, value) %>%
    mutate(data_type = "cat")

  
  if(inc_missing == FALSE){
    
    out <- out %>%
     dplyr::filter(!is.na(category))
    
  }
  
  return(out)
  
}

#' Performs the initial formatting of continuous statistics
#' 
#' @param stats Exported object from dh.getStats
#' @param vars Character vector of variable names
#' @param cont_stats Character specifying which summary statistic to return
#' for continuous stats. Use 'med_iqr' to return the median and interquartile
#' range, use 'mean_sd' to return the mean and standard deviation.
#' @inc_missing Boolean specifying whether to return missing values in
#' the output. Use TRUE for yes and FALSE for no.
#' 
#' @return Tibble containing 5 columns: 'cohort', 'variable', 'category', 
#' 'value' and 'data_type'. If `inc_missing` is TRUE contains an sixth column
#' 'miss_n_perc'
#' 
#' @importFrom dplyr %>% filter mutate select
#' 
#' @noRd
.formatContStats <- function(stats, vars, cont_stats, inc_missing, round_digits){
  
  variable <- std.dev <- perc_50 <- perc_25 <- perc_75 <- missing_n <-
    missing_perc <- cohort <- category <- value <- miss_n_perc <- 
    data_type <- NULL
  
  out <- stats$continuous %>%
    dplyr::filter(variable %in% vars) %>%
    mutate(across(c(mean:perc_95, missing_perc), ~round(., round_digits))) %>%
    mutate(value = NA, category = "") %>%
    mutate(data_type = "cont")

  
  if(cont_stats == "mean_sd"){
    
    out <- out %>%
      mutate(value = paste0(mean, " (", std.dev, ")")) 
    
  } else if(cont_stats == "med_iqr"){
    
    out <- out %>%
    mutate(value = paste0(perc_50, " (", perc_25, ",", perc_75, ")")) 
      
  }
  
  if(inc_missing == TRUE){
    
    out <- out %>%
      mutate(miss_n_perc = paste0(missing_n, " (", missing_perc, ")")) %>%
      dplyr::select(cohort, variable, category, value, miss_n_perc, data_type)
    
  } else if(inc_missing == FALSE){
    
    out <- out %>% 
      dplyr::select(cohort, variable, category, value, data_type)
    
  }
  
  return(out)
  
}

#' Checks whether the object provided is either a tibble or data frame.
#' 
#' @param labs Object provided either to var_labs, cat_labs or coh_labs
#'  
#' @return Returns an error of provided variables is not one of these types.
#' Otherwise nothing is returned.
#' 
#' @noRd 
.checkLabType <- function(labs){
  
  check_type <- class(labs)
  
  if(!any(check_type %in% c("tbl_df", "tbl", "data.frame") == TRUE) == TRUE){
    
    stop("Label object must be class tibble or data frame")
    
  }
  
}

#' Checks that all variables provided to `vars` exist in `var_labs`.
#' 
#' @param vars Character vector of variable names
#' @param var_labs Tibble with two columns: 'variable' containing the 
#' names of the variables specified in `vars`, and 'var_label' containing the
#' replacement labels for these variables.
#' 
#' @return Returns an error if all variables provided to `vars` do not exist in
#' `var_labs`. Otherwise nothing is returned.
#' 
#' @noRd
.checkLabsMatchVars <- function(vars, var_labs){
  
  missing_vars <- vars[!vars %in% var_labs$variable]
  
  if(length(missing_vars > 0)){
    
    stop(
      cat("The following variables are specifed in `vars` but do not have a 
         corresponding label provided in `var_labs`\n\n", missing_vars))
    
  }
  
}

#' Checks that all categorical variables provided to `stats_cat` have 
#' corresponding labels provided in `cat_labs`
#' 
#' @param stats_cat Object returned from .formatCatStats
#' @param cat_labs Tibble with three columns: 'variable' containing the 
#' names of the categorical variables specified in `vars`, 'category' 
#' containing the categories of these variabels, and "cat_label" containing
#' the replacement category labels for these variables.
#' 
#' @return Returns an error if labels have not been provided for all levels of
#' all categorical variables. 
#' 
#' @importFrom dplyr left_join %>% filter 
#' 
#' @noRd
.checkLabsMatchCats <- function(stats_cat, cat_labs){
  
  category <- cat_label <- NULL
  
  test_cats <- left_join(stats_cat, cat_labs, by = c("variable", "category"))
  
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

#' Checks that the columns provided in labs contain all and only those provided
#' in `required_cols`
#' 
#' @param labs Object provided either to var_labs, cat_labs or coh_labs
#' @param ncol Integer specifying the required number of columns
#' @param required_cols Character vector specifying the required column names
#' 
#' @return Returns an error if number and names of columns does not match that
#' specified
#' 
#' noRd
.checkLabCols <- function(labs, n_col, required_cols){
  
  check_cols <- any(colnames(labs) %in% required_cols == FALSE)
  check_length <- length(colnames(labs))
  
  if(check_cols == TRUE | check_length != n_col){
    
    stop(
      paste0("Labels object must be a tibble containing exactly ", n_col, 
             " columns: ", paste0(required_cols, collapse = ", ")))
    
  }
  
}

#' Makes character vector of cohorts present within `stats`
#' 
#' @param stats Exported object from dh.getStats
#' 
#' @return Character vector of cohort names
#' 
#' @importFrom dplyr %>% pull unique
#' 
#' @noRd
.availCoh <- function(stats){
  
  cohort <- NULL 
  
out <- stats %>%
  map(function(x){x %>% pull(cohort) %>% unique}) %>%
  unlist %>%
  unique

return(out)

}

#' Checks that all cohorts contained within `stats` exist in `coh_labs`.
#' 
#' @param stats Exported object from dh.getStats
#' @param coh_labs Tibble with two columns: 'cohort' containing the 
#' names of the cohorts present in `stats`, and 'coh_label' containing the
#' replacement labels for these cohorts
#' 
#' @return Returns an error if all cohorts present in `stats` do not exist in
#' `coh_labs`. Otherwise nothing is returned.
#' 
#' @noRd
.checkLabsMatchCoh <- function(stats, coh_labs){
  
  avail_coh <- .availCoh(stats)
  
  missing_coh_labs <- avail_coh[!avail_coh %in% coh_labs$cohort]
  
  if(length(missing_coh_labs > 0)){
    
    stop(
      cat("The following cohorts are contained in `stats` but do not have a 
         corresponding label provided in `coh_labs`\n\n", missing_coh_labs))
    
  }
  
}

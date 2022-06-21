#' Splits a continuous variables into four quartiles
#'
#' For some sensitivity analyses you may want to examine potential non-linear
#' associations between an exposure and outcome. This function splits a 
#' continuous variable into four continuous variables representing each 
#' quartile of the original variable. Participants with no measurement within
#' a quartile will be assigned NA. 
#'
#' @template df
#' @param var Character specifying continuous variable to transform into 
#' quartiles.
#' @param type Character specifying whether to derive quartiles from combined
#' data or within each cohort. Use "combine" to use combined quartiles, and 
#' "split" to use cohort-specific quartiles.
#' @template new_obj
#' @param var_suffix Character specifying the suffix to give the created variable.
#' Default is "_q_"
#' @template conns
#'
#' @return Servside dataframe in containing a maximum of four additional 
#' variables representing the quantiles of the original variable. If a cohort
#' has insufficient observations within that quartile (less than the filter
#' threshold) the variable will not be created an a warning will be returned.
#' 
#' @importFrom rlang arg_match set_names
#' @importFrom dplyr bind_rows group_by group_keys group_split left_join 
#' mutate select ungroup filter filter_at
#' @importFrom dsBaseClient ds.assign ds.dataFrame ds.ls ds.recodeValues
#' @importFrom DSI datashield.aggregate datashield.connections_find
#' @importFrom purrr imap map pmap
#' 
#' @family data manipulation functions
#'
#' @md
#'
#' @export
dh.quartileSplit <- function(
  df = NULL, var = NULL, new_obj = NULL, var_suffix = "_q_", type = NULL, 
  conns = NULL){
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  . <- suffix <- boole_name <- subset_name <- cohort <- enough_obs <- NULL
  
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(var)) {
    stop("`var` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(new_obj)) {
    new_obj <- df
  }

  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  }
  
  type <- ifelse(type == "combined", "combine", type)
  
  type <- arg_match(type, c("combine", "split"))
  
  cally <- call("classDS", paste0(df, "$", var))
  var_class <- DSI::datashield.aggregate(conns, cally)
  
  if (length(unique(var)) > 1) {
    stop("`var` does not have the same class in all studies.", call. = FALSE)
  } 
  
  if (any(!var_class %in% c("numeric", "integer"))) {
    stop("`age_var` must be class numeric or integer.", call. = FALSE)
  }
  
  message("** Step 1 of 4: Checking input data ... ", appendLF = FALSE)  

  start_objs <- ds.ls(datasources = conns)
  
  available_var <- .checkDataAvailable(
    df = df,
    var = var,
    conns = conns) %>%
    dplyr::filter_at(vars(-cohort), all_vars(. == FALSE))
  
  valid_coh <- available_var$cohort
  message("DONE", appendLF = TRUE)
  
  message("** Step 2 of 4: Defining subsets ... ", appendLF = FALSE)
  
  if(type == "combine"){
    
    quant_bands <- .getQuantileBands(
      df = df,
      var = var, 
      type = "combine", 
      conns = conns[valid_coh])
    
    boole_ref <- tibble(
      values = quant_bands %>% dplyr::select(perc_25:perc_75) %>% as.numeric,
      op = rep(">=", 3), 
      boole_short = paste0("bl_", 1:3))
    
    boole_ref %>%
      pmap(function(values, op, boole_short){
        ds.Boole(
          V1 = paste0(df, "$", var), 
          V2 = values, 
          Boolean.operator = op,
          newobj = boole_short, 
          datasources = conns[valid_coh])
      })

  } else if(type == "split"){
    
    quant_bands <- .getQuantileBands(
      df = df,
      var = var, 
      type = "split", 
      conns = conns[valid_coh])
    
    boole_ref <- quant_bands %>%
      dplyr::select(cohort, bl_2 = perc_25, bl_3 = perc_50, bl_4 = perc_75) %>%
      pivot_longer(
        cols = bl_2:bl_4,
        names_to = "boole_short", 
        values_to = "values") %>%
      mutate(op = ">=")
    
    boole_extra <- boole_ref %>%
      group_by(cohort) %>%
      group_split %>%
      map(~slice(., 1)) %>%
      bind_rows %>%
      mutate(boole_short = "bl_1") %>%
      mutate(op = "<")
    
    boole_ref <- bind_rows(
      boole_ref, boole_extra) %>%
      arrange(cohort, boole_short) 
    
    boole_ref %>%
      pmap(function(cohort, values, boole_short, op) {
        ds.Boole(
          V1 = paste0(df, "$", var), 
          V2 = values, 
          Boolean.operator = op,
          newobj = boole_short, 
          datasources = conns[cohort])
      })
    
  }
  
  message("DONE", appendLF = TRUE)
  
  message("** Step 2 of 3: Creating variables ... ", appendLF = FALSE)
  
  out_obj <- paste0(var, var_suffix)
  
  ds.make(
    toAssign = "bl_1+bl_2+bl_3", 
    newobj = out_obj, 
    datasources = conns[valid_coh])
  
  ds.asFactor(out_obj, out_obj, datasources = conns[valid_coh])
  
  ds.dataFrame(
    x = c(df, out_obj), 
    newobj = new_obj, 
    datasources = conns[valid_coh])
  
  message("DONE", appendLF = TRUE)
  
  message("** Step 3 of 3: Removing temporary objects ... ", appendLF = FALSE)
  
  .removeTempObjs(
    start_objs = start_objs,
    others_to_keep = new_obj,
    conns = conns)
  
  message("DONE", appendLF = TRUE)
  
  cat(
    "\nVariable ", "'", out_obj, "'",
    " has been created in dataframe ", "'", new_obj, "'", " containing 
    quartiles of ", var, ":\n\n",
    sep = "")
  
}

#' Extracts quartiles and returns them in a tibble
#'
#' @template df
#' @param var Variable for which to calculate quantiles.
#' @param type Character specifying how to compute quartiles. Use "combined"
#' to use combined quantiles across the cohorts, use "split" to calculate 
#' cohort-specific quantiles.
#' @template conns 
#'
#' @noRd
.getQuantileBands <- function(df = NULL, var = NULL, type = NULL, conns = NULL){
  
  type <- arg_match(type, c("combine", "split"))
  
  cohort <- perc_5 <- perc_25 <- perc_50 <- perc_75 <- perc_95 <- quant_names <- 
    NULL
  
  stats <- dh.getStats(
    df = df,
    vars = var, 
    checks = FALSE, 
    conns = conns)
  
  if(type == "combine"){
    
    quants <- stats$continuous %>% 
      dplyr::filter(cohort == "combined") %>%
      dplyr::select(cohort, perc_5, perc_25, perc_50, perc_75, perc_95)
    
    out <- quants %>%
      mutate(
        lower = list(c(perc_5*-100000, perc_25, perc_50, perc_75)),
        upper = list(c(perc_25, perc_50, perc_75, perc_95*100000)))
    
    } else if(type == "split"){
    
    quants <- stats$continuous %>% 
      dplyr::filter(cohort != "combined") %>%
      dplyr::select(cohort, perc_5, perc_25, perc_50, perc_75, perc_95) %>%
      group_by(cohort) 
    
    quants_names <- group_keys(quants) %>% unlist
    
    lower_list <- quants %>%
      group_split() %>%
      map(~c(.x$perc_5 * -100000, .x$perc_25, .x$perc_50, .x$perc_75)) %>%
      set_names(quant_names)
    
    upper_list <- quants %>%
      group_split() %>%
      map(~c(.x$perc_25, .x$perc_50, .x$perc_75, .x$perc_95*100000)) %>%
      set_names(quant_names)
    
    out <- quants %>%
      ungroup() %>%
      mutate(
        lower = lower_list,
        upper = upper_list)
    
  }
  
  return(out)
  
}

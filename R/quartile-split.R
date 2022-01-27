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
#' @param band_action Character specifying how the quartiles are separated:
#' * "g_l" = greater than the lowest band and less than the highest band
#' * "ge_le" = greater or equal to the lowest band and less than or equal to the
#' highest band
#' * "g_le" = greater than the lowest band and less than or equal to the highest
#' band
#' * "ge_l" = greater than or equal to the lowest band and less than the highest
#' band
#' @param type Character specifying whether to derive quartiles from combined
#' data or within each cohort. Use "combined" to use combined quartiles, and 
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
  df = NULL, var = NULL, new_obj = NULL, band_action = NULL, 
  type = NULL, var_suffix = "_q_", conns = NULL){
  
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

  if (is.null(band_action)) {
    stop("`band_action` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  }
  
  type <- ifelse(type == "combined", "combine", type)
  
  type <- arg_match(type, c("combine", "split"))
  band_action <- arg_match(band_action, c("g_l", "ge_le", "g_le", "ge_l"))
  
  cally <- call("classDS", paste0(df, "$", var))
  var_class <- DSI::datashield.aggregate(conns, cally)
  
  if (length(unique(var)) > 1) {
    stop("`var` does not have the same class in all studies.", call. = FALSE)
  } 
  
  if (any(!var_class %in% c("numeric", "integer"))) {
    stop("`age_var` must be class numeric or integer.", call. = FALSE)
  }
  
  message("** Step 1 of 9: Checking input data ... ", appendLF = FALSE)  

  start_objs <- ds.ls(datasources = conns)
  
  available_var <- .checkDataAvailable(
    df = df,
    var = var,
    conns = conns) %>%
    dplyr::filter_at(vars(-cohort), all_vars(. == FALSE))
  
  valid_coh <- available_var$cohort
  message("DONE", appendLF = TRUE)
  
  message("** Step 2 of 9: Defining subsets ... ", appendLF = FALSE)
  
  op_symbol <- .convertBooleText(band_action)
  
  if(type == "combine"){
    
    quant_bands <- .getQuantileBands(
      df = df,
      var = var, 
      type = "combine", 
      conns = conns[valid_coh])
    
    boole_ref <- .makeBooleRef(
      lower_vals = unlist(quant_bands$lower),
      lower_op = op_symbol[1],
      upper_vals = unlist(quant_bands$upper),
      upper_op = op_symbol[2])
    
    boole_ref %>%
      pmap(function(value_1, op_1, value_2, op_2, boole_short, ...) {
        .BooleTwoConditions(
          df = df,
          var = var,
          value_1 = value_1,
          op_1 = op_1,
          value_2 = value_2,
          op_2 = op_2,
          newobj = boole_short,
          conns = conns[valid_coh]
        )
      })
    
  } else if(type == "split"){
    
    quant_bands <- .getQuantileBands(
      df = df,
      var = var, 
      type = "split", 
      conns = conns[valid_coh])
    
    boole_ref <- quant_bands %>%
      pmap(function(cohort, lower, upper, ...){
        
        .makeBooleRef(
          lower_vals = unlist(lower),
          lower_op = op_symbol[1],
          upper_vals = unlist(upper),
          upper_op = op_symbol[2]) 
      }) %>% set_names(quant_bands$cohort) %>%
      bind_rows(.id = "cohort")
    
    boole_ref %>%
      pmap(function(cohort, value_1, op_1, value_2, op_2, boole_short, ...) {
        .BooleTwoConditions(
          df = df,
          var = var,
          value_1 = value_1,
          op_1 = op_1,
          value_2 = value_2,
          op_2 = op_2,
          newobj = boole_short,
          conns = conns[cohort]
        )
      })
  }
  
  message("DONE", appendLF = TRUE)
  
  message("** Step 3 of 9: Check for disclosure issues ... ", appendLF = FALSE)
  
  if(type == "combine"){
    
    discloure_ref <- boole_ref$boole_short %>%
      map(
        ~ .checkDisclosure(
          bin_vec = .x,
          conns = conns[valid_coh])) %>%
      bind_rows()
    
  } else if(type == "split"){
    
    discloure_ref <- boole_ref %>%
      pmap(function(cohort, boole_short, ...){
        .checkDisclosure(
          bin_vec = boole_short,
          conns = conns[cohort])
      }) %>%
      bind_rows()
    
  }
  
  if (nrow(discloure_ref) < 1) {
    stop("No subsets can be created as they would all contain fewer rows than the disclosure filter value")
  }
  
  failed_disclosure <- discloure_ref %>%
    left_join(., boole_ref, by = "boole_short") %>%
    dplyr::filter(enough_obs == FALSE)
  
  if (nrow(failed_disclosure) > 1) {
    warning(
      "The following subsets cannot be created as they would contain fewer observations
      than the disclosure filter value: \n\n",
      paste0(failed_disclosure$cohort, ": ", failed_disclosure$subset_name, sep = "\n")
    )
  }
  message("DONE", appendLF = TRUE)
  
  message("** Step 4 of 9: Creating variables ... ", appendLF = FALSE)
  
  if(type == "combine"){
    
    quant_ref <- left_join(boole_ref, discloure_ref, by = "boole_short") %>%
      dplyr::filter(enough_obs == TRUE) %>%
      mutate(var_name = paste0(var, var_suffix, suffix))
    
  } else if(type == "split"){
    
    quant_ref <- left_join(boole_ref, discloure_ref, by = c("cohort", "boole_short")) %>%
      dplyr::filter(enough_obs == TRUE) %>%
      mutate(var_name = paste0(var, var_suffix, suffix))
    
  }
  
  quant_ref %>%
    pmap(function(var_name, cohort, boole_short, ...){

      ## We recode all values of 0 (ie not in the quartile) to NA      
      ds.recodeValues(
        var.name = boole_short,
        values2replace.vector = 0,
        new.values.vector = NA,
        newobj = boole_short,
        datasources = conns[cohort]
      )
      
      ## Then multiply by the original variable
      ds.assign(
        toAssign = paste0(df, "$", var, "*", boole_short), 
        newobj = var_name,
        datasources = conns[cohort]
      ) 
      
    })
  
  quant_grouped <- quant_ref %>%
    group_by(cohort) 
  
  quant_names <- group_keys(quant_grouped) %>% unlist
  
  to_join <- quant_grouped %>%
    group_split %>%
    set_names(quant_names) %>%
    map(., ~.$var_name)
  
  to_join %>%
    imap(
      ~ds.dataFrame(
        x = c(df, .x),
        datasources = conns[.y], 
        newobj = df
      )
    )
  
  message("DONE", appendLF = TRUE)
  
  message("** Step 5 of 9: Removing temporary objects ... ", appendLF = FALSE)
  
  .removeTempObjs(
    start_objs = start_objs,
    others_to_keep = new_obj,
    conns = conns
  )
  
  message("DONE", appendLF = TRUE)
  
  cat(
    "\nDataframe ", "'", new_obj, "'",
    " has been created containing the following quantiles of ", var, ":\n\n",
    sep = ""
  )
  
  print(to_join)
  
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

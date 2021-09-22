#' Transforms variables based on their interquartile range
#'
#' This function is used to scale variables by the interquartile range 
#' calulcated either within cohort or using the pooled IQR across cohorts. 
#' The formula is: value(subject) / (75th percentile - 25th percentile).  
#' #'
##' @param df datashield dataframe
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
                       conns = NULL, new_df_name = df){
  
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
    map(~datashield.aggregate(conns, call("classDS", .)))
  
  if(
    all(
      str_detect(unlist(check_cont), "numeric|integer")) == FALSE){
    stop("Can only calculate IQR for continous variables: please check class of
         provided vars")
  }
  
  ## ---- Calculate IQRs ---------------------------------------------------------
  if(type == "separate"){
    
    meds <- df_vars %>%
      map(
        ~ds.quantileMean(
          x = .,
          type = "split",
          datasources = conns)
      )
    
    names(meds) <- vars
    
    iqr <- meds %>% map_depth(., 2, ~.[["75%"]] - .[["25%"]])
    
    iqr_to_make <- iqr %>% 
      map(unlist) %>% 
      map(as.matrix) %>% 
      map(as.data.frame) %>%
      map(as_tibble, rownames = "cohort") %>%
      bind_rows(.id = "variable") %>%
      mutate(formula = case_when(
       is.na(V1) ~ paste0(df, "$", variable), 
       !is.na(V1) ~  paste0(df, "$", variable, "/", V1))) %>%
      select(variable, cohort, formula)
    
    iqr_to_make %>%
      pmap(function(variable, cohort, formula){
        ds.make(
          toAssign = formula,
          newobj = paste0(variable, "_iqr_c"), 
          datasources = conns[cohort]
        )
      })
    
    ds.dataFrame(
      x = c(df, paste0(vars, "_iqr_c")),
      newobj = new_df_name, 
      datasources = conns)
    
  } else if(type == "pooled"){
    
    ## ---- Identify cohorts which are all missing -----------------------------
    missing <- df_vars %>% map(~ds.isNA(.))
    
    missing.tib <- missing %>%
      set_names(vars) %>%
      bind_rows(.id = "variable") %>%
      pivot_longer(
        cols = -variable, 
        names_to = "cohort", 
        values_to = "missing") 
    
    formean <- missing.tib %>%
      dplyr::filter(missing == FALSE) %>%
      group_by(variable)
    
    formean <- formean %>%
      group_split %>%
      map(~.$cohort) %>%
      set_names(unlist(group_keys(formean)))
    
    meds <- formean %>%
      imap(
        ~ds.quantileMean(
          x = paste0(df, "$", .y),
          type = "combine",
          datasources = conns[.x])
      )
    
    iqr <- meds %>% map_df(~.[["75%"]] - .[["25%"]]) %>%
      pivot_longer(
        cols = everything(), 
        names_to = "variable",
        values_to = "iqr"
      )
      
    iqr_to_make <- formean %>%
      map(~tibble(cohort = .)) %>%
      bind_rows(.id = "variable") %>%
      left_join(., iqr, by = "variable") %>%
      mutate(formula = paste0(df, "$", variable, "/", iqr))
    
    full_vars <- missing.tib %>%
      select(variable, cohort) %>%
      left_join(., iqr_to_make, by = c("variable", "cohort")) %>%
      mutate(formula = ifelse(is.na(formula), paste0(df, "$", variable), formula))

    full_vars %>%
      pmap(function(cohort, variable, formula, ...){
        datashield.assign(
          conns[cohort], paste0(variable, "_iqr_p"), as.symbol(formula))
      })
    
  ds.dataFrame(
    x = c(df, paste0(vars, "_iqr_p")),
    newobj = new_df_name, 
    datasources = conns)
  
  }
  
}

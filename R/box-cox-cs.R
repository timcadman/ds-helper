#' Performs a boxcox test.
#' 
#' Performs the boxcox test to identify the transformation of the input variable
#' which results in a distribution closest to normality. Currently this function
#' can handle zero values but cannot handle negative values.
#'
#' @template df
#' @template var
#' @param lamda Character vector of powers to test. Default is -2 to 2 by 0.1 
#' intervals, where power 0 = log.
#' @param unique_id Character vector specifying unique identifier within df. 
#' Default = "child_id".
#' @param transform Logical; whether to create a serverside variable 
#' corresponding to the best transformation. Default is TRUE.
#' @param type. Character vector, either "separate" to identify the best 
#' transformation in each cohort, or "combine" to identify the transformation
#' which the highest rank on average across the cohorts in `conns`
#' @template new_obj
#' @template checks
#' @template conns
#' 
#' @importFrom DSI datashield.connections_find() datashield.assign
#' @importFrom rlang arg_match
#' 
#' @return Tibble with three columns (i) power, (ii) normality, (iii) p_value.
#' These are sorted from highest to lowest p-value, with the first row 
#' representing the best transformation. If `transform` is TRUE, also creates
#' a serverside object corresponding to the best transformation.
#' 
#' @export
#' 
#' 

dh.boxCoxCS <- function(df = NULL, var = NULL, lamda = seq(-2, 2, 0.2), 
                      unique_id = "child_id", type = NULL, transform = TRUE,
                      new_obj = NULL, checks = TRUE, conns = NULL) {
  
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(var)) {
    stop("`var` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(new_obj)) {
    stop("`new_obj` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(type)) {
    stop("`type` must not be NULL.", call. = FALSE)
  }
  
  type <- arg_match(type, c("separate", "combine"))
  
  if (is.null(transform)) {
    stop("`transform` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(conns)) {
    conns <- DSI::datashield.connections_find()
  }
  
  if (checks == TRUE) {
    .isDefined(df = df, vars = var, conns = conns)
  }
  
  df_var <- paste0(df, "$", var)

  anon.pdata <- dh.getAnonPlotData(
    df = df,
    var_1 = var,
    conns = conns,
    checks = FALSE) %>%
    dplyr::rename(value = 2)
  
  box_trans <- .doBoxCox(pdata = anon.pdata, type = type)
    
  if(transform == TRUE){
  
    .checkNegatives(df_var, conns)
    .finalBoxTrans(box_trans, df_var, type, new_obj, conns)
    
  } 
  
  .boxMessage(box_trans, type, transform)
  
  return(box_summary)
  
}

#' Performs boxcox test 
#'
#' @param formulae Tibble, output from .defineBoxTrans
#' @param type. Character vector, either "separate" to identify the best 
#' transformation in each cohort, or "combine" to identify the transformation
#' which the highest rank on average across the cohorts in `conns`
#' @template conns
#' 
#' @return Tibble with three columns (i) power, (ii) normality, (iii) p_value.
#' These are sorted from highest to lowest p-value, with the first row 
#' representing the best transformation.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>% contains
#' @importFrom purrr cross2 map_chr
#' @importFrom dsExposomeClient ds.normalityTest
#'
#' @noRd
.doBoxCox <- function(pdata, type, conns){
  
  if(type == "combine"){
    
    box_input <- pdata %>% dplyr::filter(cohort == "combined" & value >= 1)
    
    box_out <- boxcox(
      object = lm(box_input$value ~1, y = TRUE),
      lamda = lamda, 
      plotit = FALSE)
    
    out <- tibble(
      cohort = "combined", 
      lamda = box_out$x[which.max(box_out$y)])
    
  } else if(type == "separate"){
    
    box_input <- anon.pdata %>% 
      dplyr::filter(cohort != "combined" & value >= 1) %>%
      group_by(cohort) 
    
    coh_names <- group_keys(box_input) %>% pull
    
    out <- box_input %>% 
      group_split %>%
      map(function(x){
        
        reg_out <- lm(x$value ~1, y = TRUE)
        
        box_out <- boxcox(
          object = reg_out,
          lamda = lamda, 
          plotit = FALSE) 
        
        out <- tibble(
          lamda = box_out$x[which.max(box_out$y)])
        
      }) %>%
      set_names(coh_names) %>%
      bind_rows(.id = "cohort")
    
  }
  
}


#' Checks for negative values in the input vector. Currently the function 
#' cannot handle these.
#'
#' @param df_var serverside vector of variable to transform
#' @template conns
#' 
#' @return Either no return or an error message if vector contains negative 
#' values
#'
#' @importFrom DSI datashield.assign
#' @importFrom dsBaseClient ds.table
#'
#' @noRd
.checkNegatives <- function(df_var, conns){
  
  calltext <- call("BooleDS", df_var, 0, 3, "0", TRUE)
  DSI::datashield.assign(conns, "box_neg_boole", calltext)
  
  neg_test <- ds.table("box_neg_boole", useNA = "always", datasources = conns)
  
  levels <- dimnames(neg_test$output.list$TABLES.COMBINED_all.sources_counts)
  
  if(levels %in% "1" == TRUE){
    
    stop("`var` contains negative values. Unfortunately at the moment this 
         function does not work with negative values.")
  }
  
}

#' Creates transformation of variable best on best power
#'
#' @param best_trans_ref Output from .finalTransRef 
#' @param df_var serverside vector of variable to transform
#' @param type. Character vector, either "separate" to identify the best 
#' transformation in each cohort, or "combine" to identify the transformation
#' which the highest rank on average across the cohorts in `conns`
#' @template conns
#' 
#' @return Creates serverside vector containing best transformation of `var`
#'
#' @importFrom DSI datashield.assign
#' @importFrom purrr pmap
#'
#' @noRd
.finalBoxTrans <- function(box_trans, df_var, type, new_obj, conns){
  
  cally <- paste0(df_var, "+0.00005")
  datashield.assign(conns, "var_const", as.symbol(cally))
  
  if(type == "combine"){
    
    cally <- paste0("var_const^", box_trans$lamda)
      
    datashield.assign(conns, new_obj, as.symbol(cally))
    
  } else if(type == "separate"){
    
    box_trans %>%
      pmap(function(cohort, lamda){
        
        cally <- paste0("(", df_var, "+0.00005)^", lamda)
        datashield.assign(conns[cohort], new_obj, as.symbol(cally))
        
      })
    
  }
  
}

#' Returns relevant messages for boxcox function
#'
#' @param best_trans_ref Output from .finalTransRef 
#' @param type. Character vector, either "separate" to identify the best 
#' transformation in each cohort, or "combine" to identify the transformation
#' which the highest rank on average across the cohorts in `conns`
#' @param transform Logical; whether to create a serverside variable 
#' corresponding to the best transformation. Default is TRUE.
#' 
#' @return Return relevant error message
#'
#' @noRd
.boxMessage <- function(box_trans, type, transform){
  
  if(type == "separate"){
    
    message(
      cat("\nThe most normal transformation for each cohort is listed below."))
    
    print(box_trans)
    
  } else if(type == "combine"){
    
    message(
      cat("\nThe transformation ranked most normal on average across all cohorts was to power of: ", best_trans_ref))
    
  }
  
  if(transform == TRUE & type == "separate"){
    
    message(cat("\nServerside objects have been created containing the best transformation for each cohort."))
    
  } else if(transform == TRUE & type == "combine"){
    
    message(cat("\nServerside objects have been created containing this transformation."))
    
  }
  
}



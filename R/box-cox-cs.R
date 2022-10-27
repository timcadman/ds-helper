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
  var_con <- paste0(var, "_con")
  
  formulae <- .defineBoxTrans(var_con, lamda) 
  
  .makeBoxTrans(formulae, unique_id, df, var, var_con, conns)

  box_summary <- .doBoxCox(formulae, type, conns)
  
  best_trans_ref <- .finalTransRef(box_summary, type, conns)
  
  if(transform == TRUE){
  
    .finalBoxTrans(best_trans_ref, df_var, type, new_obj, conns)
    
  } 
  
  .boxMessage(best_trans_ref, type, transform)
  
  return(box_summary)
  
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

#' Creates a tibble of formula for boxcox transformations.
#'
#' @param var_con serverside vector of variable to transform
#'
#' @return Tibble with two columns (i) power, (ii) formula
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr cross2 map_chr
#'
#' @noRd
.defineBoxTrans <- function(var_con, lamda){
  
  lamda_nolog <- lamda[lamda != 0]
  lamda_form <- paste0("^", lamda_nolog)
  
  trans_out <- tibble(
    power = as.character(lamda_nolog),
    formula = cross2(var_con, lamda_form) %>% 
      map_chr(paste, sep = "", collapse = ""), 
    ref = paste0("trans_", seq(1:length(lamda_nolog))))
  
  if(any(lamda %in% 0) == TRUE){
    
    log_trans <- tibble(
      power = "log",
      formula = paste0("log(", var_con, ")"))
    
    trans_out <- bind_rows(trans_out, log_trans) 
    
    trans_out <- trans_out %>%
      mutate(ref = paste0("trans_", seq(1:nrow(trans_out))))
    
    return(trans_out)
    
  } else{
    
    return(trans_out)
    
  }
  
}

#' Creates transformations for boxcox test. A small constant is added to prevent
#' infinite values. Note, have to do a bit of a hack by creating phenotype part 
#' of object also as exposures.
#'
#' @param formulae Tibble, output from .defineBoxTrans
#' @param unique_id Character vector specifying unique identifier within df. 
#' Default = "child_id".
#' @template df
#' @template var
#' @param var_con serverside vector of variable to transform
#' @template conns
#'
#' @return Exposome serverside object names 'box_set' created containing 
#' transformations.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr pmap
#' @importFrom DSI datashield.assign
#' @importFrom dsBaseClient ds.dataFrame
#' @importFrom dsExposomeClient ds.loadExposome
#'
#' @noRd
.makeBoxTrans <- function(formulae, unique_id, df, var, var_con, conns){
  
  cally <- paste0(df, "$", var, "+0.00005")
  datashield.assign(conns, var_con, as.symbol(cally))
  
  formulae %>%
    pmap(function(formula, ref, ...){
      
      datashield.assign(conns, ref, as.symbol(formula))
      
    })
  
  ds.dataFrame(x = c(paste0(df, "$", unique_id), formulae$ref), 
               newobj = "box_ref", datasources = conns, 
               check.names = FALSE, DataSHIELD.checks = FALSE)
  
  ds.loadExposome(
    exposures = "box_ref", 
    phenotypes = "box_ref", 
    exposures.idcol = unique_id,
    phenotypes.idcol = unique_id, 
    object_name = "box_set")
  
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
.doBoxCox <- function(formulae, type, conns){
  
  exposure <- p.value <- . <- power <- normality <- p_value <- cohort <-
    rank_av <- NULL
  
  norm_test <- ds.normalityTest("box_set", datasources = conns)
  
  norm_out <- norm_test %>%
    map(as_tibble) %>%
    map(
      ~dplyr::rename(., 
                     ref = exposure,
                     p_value = p.value)) %>%
    map(~left_join(formulae, ., by = "ref")) %>%
    map(~dplyr::select(., power, normality, p_value)) %>%
    map(~arrange(., desc(p_value)))
  
  if(type == "separate"){
    
    return(norm_out)
    
  } else if(type == "combine"){
        
    norm_out <- norm_out %>%
      map(~mutate(., rank = seq(1, nrow(formulae), 1))) %>%
      bind_rows(.id = "cohort") %>%
      pivot_wider(
        names_from = cohort,
        values_from = c(rank, p_value)) %>%
      mutate(rank_av = rowSums(
        dplyr::select(., dplyr::contains("rank"))) / 
        length(dplyr::select(., contains("rank")))) %>%
      arrange(rank_av) %>%
      dplyr::select(power, normality, rank_av)
    
    return(norm_out)
    
  }
        
}

#' Creates short reference table containing best transformation(s) 
#'
#' @param box_summary Output from .doBoxCox
#' @param type. Character vector, either "separate" to identify the best 
#' transformation in each cohort, or "combine" to identify the transformation
#' which the highest rank on average across the cohorts in `conns`
#' @template conns
#' 
#' @return If type is "separate" returns a tibble with two columns: (i) cohort
#' and (ii) power summarising the best transformation in each cohort. If type is
#' "combine" returns a single character stating the best overall transformation.
#'
#' @importFrom dplyr %>% slice bind_rows select
#' @importFrom purrr map
#'
#' @noRd
.finalTransRef <- function(box_summary, type, conns){
  
  cohort <- power <- NULL
  
  if(type == "combine"){
    
    trans_ref <- box_summary$power[[1]]
  
   }else if(type == "separate"){
      
      trans_ref <- box_summary %>%
        map(~slice(., 1)) %>%
        bind_rows(.id = "cohort") %>%
        dplyr::select(cohort, power)
      
    }
    
    return(trans_ref)
  
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
.finalBoxTrans <- function(best_trans_ref, df_var, type, new_obj, conns){
  
  cally <- paste0(df_var, "+0.00005")
  datashield.assign(conns, "var_const", as.symbol(cally))
  
  if(type == "combine"){
    
    if(best_trans_ref == "log"){
      
      cally <- "log(var_const)"
      
    } else{
      
      cally <- paste0("var_const^", best_trans_ref)
      
    }
    
    datashield.assign(conns, new_obj, as.symbol(cally))
    
  } else if(type == "separate"){
    
    best_trans_ref %>%
      pmap(function(cohort, power){
        
        cally <- paste0("(", df_var, "+0.00005)^", power)
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
.boxMessage <- function(best_trans_ref, type, transform){
  
  if(type == "separate"){
    
    message(
      cat("\nThe most normal transformation for each cohort is listed below."))
    
    print(best_trans_ref)
    
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



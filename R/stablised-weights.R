#' Generate Stabilized Weights Using ds.glmSLMA output and outcome proportions.
#' 
#' This calculates stablised inverse probability weights. Currently only works with a binary
#' outcome variable.
#'
#' @param glm_object A fitted ds.glmSLMA object.
#' @param responder_df A serverside data frame containing the responder variable.
#' @param responder_var Character vector of outcome variable within `responder_df`.
#' @param responder_level Character vector indicating level of `responder_var` corresponding to
#' responders.
#' @param non_responder_level Character vector indicating level of `responder_var` corresponding to
#' non-responders.
#' @template conns 
#' @param new_obj A character string for the name of the new object to store stabilized weights.
#' @details The function performs the following steps:
#'   1. Generate predicted values from the GLM model.
#'   2. Calculate proportions of a binary outcome using the outcome data.
#'   3. Format the proportions data for analysis.
#'   4. Create stabilized weights using the formatted proportions and GLM predictions.
#' @note Currently only works with binary outcome variable.
#' @return Stabilized weights assigned as serverside object `new_obj`
#' @importFrom checkmate assert_character assert_true
#' @export
dh.stablisedWeights <- function(glm_object = NULL, responder_df = NULL, responder_var = NULL,
                                non_responder_level, responder_level, new_obj = NULL, conns = NULL){
  
  stablise_check_args(glm_object, responder_df, responder_var, non_responder_level, responder_level, 
                      new_obj, conns)
    
  predict_values(glm_object)
  
  proportions <- get_proportions(responder_df, responder_var)
  
  formatted_proportions <- formatProportions(proportions, conns, non_responder_level, responder_level)
  
  make_stablised_weights_responders(formatted_proportions, glm_object, responder_level, conns)
  make_stablised_weights_non_responders(formatted_proportions, glm_object, non_responder_level, conns)
  
  combine_weight_objects(responder_df, responder_var, new_obj, conns)
  
}

#' Validate and Check Arguments for Stabilized Weights Calculation
#'
#' This function validates and checks the arguments provided for calculating stabilized weights.
#'
#' @param glm_object A fitted ds.glmSLMA object.
#' @param outcome_df A serverside data frame containing the outcome variable.
#' @param outcome_var Character vector of outcome variable within `outcome_df`.
#' @param new_obj A character string for the name of the new object to store stabilized weights.
#' @template conns
#' @details The function performs several checks, including:
#'   1. Verifying that `outcome_df`, `outcome_var`, and `conns` are defined.
#'   2. Checking if the `glm_object` exists.
#'   3. Confirming that the outcome variable has exactly two levels (binary).
#'   4. Ensuring that `new_obj` is a character string.
#' @return Throws error(s) if any check fails, else no return.
#' @noRd
stablise_check_args <- function(glm_object, responder_df, responder_var, non_responder_level, 
                                responder_level, new_obj, conns){
  
  responder_objs_exist <- unlist(.isDefined(responder_df, responder_var, conns))
  glm_object_exists <- check_glm_exists(glm_object)
  responder_levels <- get_responder_levels(responder_df, responder_var, conns)
  
  error_messages <- makeAssertCollection()
  
  checkmate::assert_true(all(responder_objs_exist), add = error_messages)
  checkmate::assert_true(all(glm_object_exists), add = error_messages)
  checkmate::assert_true(length(responder_levels) == 2, add = error_messages)
  checkmate::assert_set_equal(responder_levels, c(non_responder_level, responder_level), 
                              add = error_messages)
  checkmate::assert_character(new_obj, add = error_messages)
  
  return(reportAssertions(error_messages))
  
}

#' Extracts unique levels of responder variable within a specified data frame
#'
#' @param responder_df A serverside data frame containing the responder variable.
#' @param responder_var Character vector of outcome variable within `responder_df`.
#' @param responder_level Character vector indicating level of `responder_var` corresponding to
#' responders.
#' @param non_responder_level Character vector indicating level of `responder_var` corresponding to
#' non-responders.
#'
#' @return Character vector indicating levels of responder variable
#' @noRd 
get_responder_levels <- function(responder_df, responder_var, non_responder_level, 
                                           responder_level, conns){
  
cally <- paste0("levelsDS(", responder_df, "$", responder_var, ")")
lvls <- datashield.aggregate(conns, as.symbol(cally))[[1]]$Levels

return(lvls)

}

#' Check the Existence of a ds.glmSLMA object.
#'
#' @param glm_object A fitted ds.glmSLMA object.
#' @details The function determines the existence of a ds.glmSLMA object and returns a list of 
#' logical values indicating whether the object is present in each cohort.
#' @return Character vector length `conns` of logical values (TRUE or FALSE) indicating whether the 
#' object exists.
#' @importFrom dsBaseClient ds.exists
#' 
#' @noRd
check_glm_exists <- function(glm_object){
  
  return(unlist(ds.exists(glm_object)))
  
}


#' Check the number of levels in input outcome variable
#'
#' @param outcome_df A serverside data frame containing the outcome variable.
#' @param outcome_var The name of the outcome variable in the data frame.
#' @details The function computes the number of levels (categories) in the specified binary outcome
#'   variable and returns this count.
#' @return A list of length of the length of conns indicating the umber of levels (categories) in 
#' the outcome variable.
#' @noRd
check_two_levels <- function(responder_df, responder_var){
  
  outcome_levels <- ds.levels(paste0(responder_df, "$", responder_var))
  n_levels <- outcome_levels %>% map(~length(.x$Levels))
  return(unlist(n_levels))
  
}
  
  #' Generate predicted values from a Generalized Linear Model (GLM) object.
  #'
  #' @param glm_object A fitted ds.glmSLMA object.
  #' @details The function takes a ds.glmSLMA model that has been previously fitted to the data.
  #' @return A serverside object with the predicted values, stored with a name
  #'   based on the original ds.glmSLMA object suffixed with "_pred."
  #' @importFrom dsBaseClient ds.glmPredict
  #' @note The default "output.type" is set to "response," providing the predicted response values
  #'   for the model.
  #'
#' @noRd
predict_values <- function(glm_object){
    ds.glmPredict(
      glmname = glm_object, 
      output.type = "response", 
      newobj = paste0(glm_object, "_pred")
    )
  }
  
  #' Calculate Proportions of a binary outcome
  #'
  #' @param outcome_df A serverside data frame containing the outcome variable.
  #' @param outcome_var The name of the outcome variable in the data frame.
  #' @details The function computes the proportions of values in the specified outcome
  #   variable and accounts for missing values ("NA") if any.
  #' @return A table containing the proportions.
  #'
  #' @noRd
  get_proportions <- function(responder_df, outcome_var){
    
    proportions <- ds.table(
      paste0(responder_df, "$", outcome_var), 
      useNA = "always")
    
    return(proportions)
    
  }
  
  #' Format Proportions Data
  #'
  #' This function formats proportions data, typically generated from a prior calculation,
  #' into a structured and user-friendly format.
  #'
  #' @param proportions Output from ds.table returned by `.getProportions`.
  #' @template conns
  #' @details The function processes the proportions data.
  #' @return A tibble with three columns (i) level,  (ii) cohorts and (iii) proportions providing 
  #' the proportion in the exposed category. This is then used to calculate the stablised weights.
  #' @noRd
  formatProportions <- function(proportions, conns, non_responder_level, responder_level){
    
    level <- NULL
    
    formatted <- proportions$output.list$TABLE_rvar.by.study_col.props %>%
      as_tibble(rownames = "level") %>%
      set_names(c("level", names(conns))) %>%
      dplyr::filter(level %in% c(non_responder_level, responder_level)) %>%
      pivot_longer(
        cols = names(conns), 
        names_to = "cohort", 
        values_to = "proportion")
    
    return(formatted)
    
  }
  
  #' Create stabilized weights for responders from formatted proportions data and GLM predictions.
  #'
  #' @param formatted A tibble containing formatted proportions data, output from `.formatProportions`
  #' @param glm_object The Generalized Linear Model (GLM) object used for prediction.
  #' @param responder_level Character vector indicating level of `outcome_var` corresponding to
  #' responders.
  #' @param new_obj A character string for the name of the new object to store stabilized weights.
  #' @details The function uses the formatted proportions data and ds.glmSLMA predictions
  #   to create stabilized weights.
  #' @return Stabilized weights saved as a serverside object defined by `new_name`.
  #'
  #' @noRd
make_stablised_weights_responders <- function(formatted, glm_object, responder_level, conns){
  
  level <- NULL
  
  formatted %>% 
    dplyr::filter(level == responder_level) %>%
    pmap(function(cohort, proportion, ...){
      
      responder_formula = paste0(proportion, "/", paste0(glm_object, "_pred$fit"))
      
      ds.assign(
        responder_formula, 
        datasources = conns[cohort], 
        newobj = "resp_weights")
      
    })
  
}
  
#' Create stabilized weights for non-responders from formatted proportions data and GLM predictions.
#'
#' @param formatted A tibble containing formatted proportions data, output from `.formatProportions`
#' @param glm_object The Generalized Linear Model (GLM) object used for prediction.
#' @param non-responder-level Character vector indicating level of `outcome_var` corresponding to
#' non-responders.
#' @param new_obj A character string for the name of the new object to store stabilized weights.
#' @details The function uses the formatted proportions data and ds.glmSLMA predictions
#   to create stabilized weights.
#' @return Stabilized weights saved as a serverside object defined by `new_name`.
#'
#' @noRd
  make_stablised_weights_non_responders <- function(formatted, glm_object, non_responder_level, 
                                                    conns){
    level <- NULL
    
    formatted %>% 
      dplyr::filter(level == non_responder_level) %>%
      pmap(function(cohort, proportion, ...){
        
        non_responder_formula = paste0(proportion, "/(1-", paste0(glm_object, "_pred$fit"), ")")
        
        ds.assign(
          non_responder_formula, 
          datasources = conns[cohort], 
          newobj = "non_resp_weights")
        
      })
    
  }
  
  
#' @title Combine serverside weights objects into one variable.
#'
#' @param responder_df A serverside data frame containing the responder variable.
#' @param responder_var Character vector of outcome variable within `responder_df`.
#' @template new_obj
#' @template conns
#' @return
#' Numeric serverside object with name `new_obj` created from the weights objects for responders and
#' non-responders.
#'
#'@noRd
combine_weight_objects <- function(responder_df, responder_var, new_obj, conns){
  
  responder_level <- non_responder_level <- NULL
  
  ds.Boole(
    V1 = paste0(responder_df, "$", responder_var), 
    V2 = responder_level, 
    Boolean.operator = "==", 
    newobj = "resp_y_n")
  
  ds.Boole(
    V1 = paste0(responder_df, "$", responder_var), 
    V2 = non_responder_level, 
    Boolean.operator = "==", 
    newobj = "non_resp_y_n")
  
  ds.assign("resp_weights*resp_y_n", "resp_w_clean")
  ds.assign("non_resp_weights*non_resp_y_n", "non_resp_w_clean")
  ds.assign("resp_w_clean+non_resp_w_clean", new_obj)
  
}
  
  
  
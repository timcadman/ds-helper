#' Generate Stabilized Weights Using ds.glmSLMA output and outcome proportions.
#' 
#' This calculates stablised inverse probability weights. Currently only works with a binary
#' outcome variable.
#'
#' @param glm_object A fitted ds.glmSLMA object.
#' @param outcome_df A serverside data frame containing the outcome variable.
#' @param outcome_var Character vector of outcome variable within `outcome_df`.
#' @template conns 
#' @param new_obj A character string for the name of the new object to store stabilized weights.
#' @details The function performs the following steps:
#'   1. Generate predicted values from the GLM model.
#'   2. Calculate proportions of a binary outcome using the outcome data.
#'   3. Format the proportions data for analysis.
#'   4. Create stabilized weights using the formatted proportions and GLM predictions.
#' @note Currently only works with binary outcome variable.
#' @return Stabilized weights assigned as serverside object `new_obj`
#' @export
dh.stablisedWeights <- function(glm_object = NULL, outcome_df = NULL, outcome_var = NULL, 
                                new_obj = NULL, conns = NULL){
  
    stablise_check_args(glm_object, outcome_df, outcome_var, new_obj, conns)
    
  predictedValues(glm_object)
  
  proportions <- getProportions(
    outcome_df = outcome_df, 
    outcome_var = outcome_var)
  
  formatted <- formatProportions(
    proportions = proportions, 
    conns = conns)
  
  makeStablisedWeights(formatted, glm_object, new_obj, conns)
  
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
#' @importFrom checkmate check_character check_true
#' @noRd
stablise_check_args <- function(glm_object, outcome_df, outcome_var, new_obj, conns){
  
  .isDefined(outcome_df, outcome_var, conns)
  
  glm_object_exists <- check_glm_exists(glm_object)
  outcome_levels <- check_two_levels(outcome_df, outcome_var)
  
  error_messages <- makeAssertCollection()
  
  assert(
    check_character(glm_object),
    check_true(all(glm_object_exists)), 
    check_true(all(outcome_levels == 2)),
    check_character(new_obj),
    add = error_messages, 
    combine = "and")
  
  return(reportAssertions(error_messages))
  
}

#' Check the Existence of a ds.glmSLMA object.
#'
#' @param glm_object A fitted ds.glmSLMA object.
#' @details The function determines the existence of a ds.glmSLMA object and returns a list of 
#' logical values indicating whether the object is present in each cohort.
#' @return A list of length `conns of logical values (TRUE or FALSE) indicating whether the object 
#' exists.
#' @importFrom dsBaseClient ds.exists
#' @noRd
check_glm_exists <- function(glm_object){
  
  unlist(ds.exists(glm_object))
  
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
check_two_levels <- function(outcome_df, outcome_var){
  
  outcome_levels <- ds.levels(paste0(outcome_df, "$", outcome_var))
  n_levels <- outcome_levels %>% map(~length(.x$Levels))
  return(n_levels)
  
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
  predictedValues <- function(glm_object){
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
  getProportions <- function(outcome_df, outcome_var){
    
    proportions <- ds.table(
      paste0(outcome_df, "$", outcome_var), 
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
  #' @return A tibble with two columns (i) cohorts and (ii) proportions providing the proportion
  #' in the exposed category. This is then used to calculate the stablised weights.
  #' @noRd
  formatProportions <- function(proportions, conns){
    
    level <- NULL
    
    formatted <- proportions$output.list$TABLE_rvar.by.study_col.props %>%
      as_tibble(rownames = "level") %>%
      set_names(c("level", names(conns))) %>%
      dplyr::filter(level == "1") %>%
      pivot_longer(
        cols = names(conns), 
        names_to = "cohort", 
        values_to = "proportion") %>%
      dplyr::select(-level)
    
    return(formatted)
    
  }
  
#' Create stabilized weights from formatted proportions data and GLM predictions.
#'
#' @param formatted A tibble containing formatted proportions data, output from `.formatProportions`
#' @param glm_object The Generalized Linear Model (GLM) object used for prediction.
#' @param new_obj A character string for the name of the new object to store stabilized weights.
#' @details The function uses the formatted proportions data and ds.glmSLMA predictions
#   to create stabilized weights.
#' @return Stabilized weights saved as a serverside object defined by `new_name`.
#'
  #' @noRd
makeStablisedWeights <- function(formatted, glm_object, new_obj, conns){
  
  formatted %>%
    pmap(function(cohort, proportion){
      
      ds.assign(
        paste0(proportion, "/", paste0(glm_object, "_pred$fit")), 
        datasources = conns[cohort], 
        newobj = new_obj)
      
    })
  
}
#' Internal function to check whether object exists.
#'
#' This is a wrapper around isDefined with more flexibility.
#' It always checks if the object exist, and optionally checks if variables
#' exist within that object.
#'
#' @param df dataframe on armadillo/opal server
#' @param vars optional variables within that object
#' @param conns datashield connections object
#'
#' @importFrom utils getFromNamespace
#'
#' @noRd
.isDefined <- function(df = NULL, vars = NULL, conns = NULL) {
  isDefined(obj = df, datasources = conns)
  
  if (!is.null(vars)) {
    paste0(df, "$", vars) %>%
      map(~ isDefined(obj = .x, datasources = conns))
  }
}

#' @title Splits character by '$' and returns the single characters
#' @description This is an internal function.
#' @details Not required
#' @param input a vector or a list of characters
#' @keywords internal
#' @return a vector of characters
#'
extract <- function(input) {
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)) {
    inputterms <- unlist(strsplit(input[i], "\\$", perl = TRUE))
    if (length(inputterms) > 1) {
      obj1 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][2]
    } else {
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list("holders" = output1, "elements" = output2)
  return(output)
}

#'
#' @title Checks if the objects are defined in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be defined (i.e. exists)
#' in all the studies. If not the process should halt.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified, the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param obj a character vector, the name of the object(s) to look for.
#' @param error.message a Boolean which specifies if the function should stop and return
#' an error message when the input object is not defined in one or more studies or to
#' return a list of TRUE/FALSE indicating in which studies the object is defined
#' @keywords internal
#' @return returns an error message if \code{error.message} argument is set to TRUE (default)
#' and if the input object is not defined in one or more studies, or a Boolean value if
#' \code{error.message} argument is set to FALSE.
#' @author Demetris Avraam for DataSHIELD Development Team
#'
isDefined <- function(datasources = NULL, obj = NULL, error.message = TRUE) {
  inputobj <- unlist(obj)
  
  for (i in 1:length(inputobj)) {
    extractObj <- extract(inputobj[i])
    
    if (is.na(extractObj$holders)) {
      cally <- call("exists", extractObj$elements)
      out <- DSI::datashield.aggregate(datasources, cally)
    } else {
      dfname <- as.name(extractObj$holders)
      cally <- call("exists", extractObj$elements, dfname)
      out <- DSI::datashield.aggregate(datasources, cally)
    }
    
    if (error.message == TRUE & any(out == FALSE)) {
      stop("The input object ", inputobj[i], " is not defined in ", paste(names(which(out == FALSE)), collapse = ", "), "!", call. = FALSE)
    } else {
      return(out)
    }
  }
}

#' Check that there is some non-missing data on provided variable.
#' This is needed so we don't try to create empty subsets later
#'
#' @param df Opal/armadillo data frame
#' @param var variable in df to check
#' @param conns datashield connections object
#'
#' @importFrom dsBaseClient ds.isNA
#' @importFrom dplyr %>% bind_rows filter pull
#' @importFrom tidyr pivot_longer
#'
#' @noRd
.checkDataAvailable <- function(df, var, conns) {
  cohort <- . <- NULL
  
  missing_col_name <- paste0(var, "_missing")
  
  check_missing <- ds.isNA(
    x = paste0(df, "$", var),
    datasources = conns
  ) %>%
    bind_rows() %>%
    pivot_longer(
      cols = everything(),
      names_to = "cohort",
      values_to = missing_col_name
    )
  
  if (any(check_missing[missing_col_name] == TRUE)) {
    warning(
      "Cohort(s) '",
      check_missing %>%
        dplyr::filter(missing_col_name == TRUE) %>%
        pull(cohort),
      "' have no available data on variable '", var, "'",
      "and will be excluded from the analysis"
    )
  }
  
  return(check_missing)
}

#' Convert strings indicating boolean operators to symbols. Used in functions
#' make-strata and quantile-split.
#'
#' @param boole_string Argument string to be converted to operators.
#' @importFrom dplyr case_when
#' @noRd
.convertBooleText <- function(boole_string){
  
  op_symbol <- case_when(
    boole_string == "g_l" ~ c(">", "<"),
    boole_string == "ge_le" ~ c(">=", "<="),
    boole_string == "g_le" ~ c(">", "<="),
    boole_string == "ge_l" ~ c(">=", "<")
  )
  
}

#' Creates a reference tibble indicating subsets & variable to create. Used in
#' make-strata and quantile-split.
#'
#' @param lower_vals Numeric vector indicating lower values of subset
#' @param lower_op Character indicating operator to use to evaluate lower
#' values
#' @param upper_vals Numeric vector indicating upper values of subset
#' @param upper Character indicating operator to use to evaluate upper values
#' @importFrom dplyr %>% mutate across
#' @importFrom tibble tibble
#' @importFrom stringr str_replace_all
#' @noRd
.makeBooleRef <- function(
  lower_vals, lower_op, upper_vals, upper_op,  boole_prefix = "boole_",
  subset_prefix = "subset_"){
  
  . <- suffix <- boole_name <- subset_name <- NULL
  
  out <- tibble(
    value_1 = lower_vals,
    op_1 = lower_op,
    value_2 = upper_vals,
    op_2 = upper_op) %>%
    mutate(
      suffix = seq(1, length(.)),
      boole_name = paste0(boole_prefix, suffix),
      subset_name = paste0(subset_prefix, suffix)) %>%
    mutate(across(
      c(boole_name, subset_name),
      ~ str_replace_all(
        string = .,
        pattern = "-",
        replacement = "m"))) %>%
    mutate(
      boole_short = paste0("bl_", seq(1, length(boole_name))), 
      subset_short = paste0("sb_", seq(1, length(subset_name)))
    )
  
}

#' Create a variable indicating whether two conditions are met.
#' ds.Boole only allows one condition to be specified.
#'
#' @param df opal/armadillo data frame
#' @param var variable in df to evaluate
#' @param value_1 first value to evaluate against
#' @param op_1 first operator for evaluation
#' @param value_2 second value to evaluate against
#' @param op_2 second operator for evaluation
#' @param newobj name of object to create indicating if both conditions are met
#' @param conns datashield connections object
#'
#' @importFrom DSI datashield.assign
#' @importFrom dplyr %>% filter select pull
#'
#' @noRd
.BooleTwoConditions <- function(df, var, value_1, op_1, value_2, op_2, newobj, conns) {
  symbol <- number <- . <- NULL
  
  op_ref <- tibble(
    symbol = c("==", "!=", "<", "<=", ">", ">="),
    number = seq(1, 6, 1)
  )
  
  get_op_number <- function(op) {
    op_ref %>%
      dplyr::filter(symbol == op) %>%
      dplyr::select(number) %>%
      pull()
  }
  
  op_1_num <- get_op_number(op = op_1)
  op_2_num <- get_op_number(op = op_2)
  
  calltext <- call("BooleDS", paste0(df, "$", var), value_1, op_1_num, "0", TRUE)
  DSI::datashield.assign(conns, "boole_1", calltext)
  
  calltext <- call("BooleDS", paste0(df, "$", var), value_2, op_2_num, "0", TRUE)
  DSI::datashield.assign(conns, "boole_2", calltext)
  
  DSI::datashield.assign(conns, newobj, as.symbol("boole_1*boole_2"))
  
}

#' Check whether a provided binary vector (output from ds.Boole) has
#' a number of cases > minimum number of rows for subsets.
#'
#' @param bin_vec a binary vector containing only 0, 1 or NA
#' @param conns datashield connections object
#'
#' @importFrom dsBaseClient ds.table ds.listDisclosureSettings
#' @importFrom dplyr %>% filter mutate left_join select
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#'
#' @noRd
.checkDisclosure <- function(bin_vec, conns) {
  observations <- . <- NULL
  
  if(utils::packageVersion("dsBaseClient") == "6.1.0"){
    
    coh_ref <- tibble(
      coh_num = as.character(seq(1, length(names(conns)))),
      cohort = names(conns)
    )
    
    n_obs <- ds.table(bin_vec, datasources = conns)$output.list$TABLE_rvar.by.study_counts %>%
      as_tibble(rownames = "levels") %>%
      pivot_longer(
        cols = c(-levels),
        names_to = "coh_num",
        values_to = "observations"
      ) %>%
      left_join(., coh_ref, by = "coh_num")
    
  } else{
    
    n_obs <- ds.table(bin_vec, datasources = conns)$output.list$TABLE_rvar.by.study_counts %>%
      as_tibble(rownames = "levels") %>%
      pivot_longer(
        cols = c(-levels),
        names_to = "cohort",
        values_to = "observations"
      ) 
    
  }
  
      
  if(n_obs %>% dplyr::filter(levels == 1) %>% nrow == 0){
    
    replace_all_missing <- n_obs %>%
      mutate(
        levels = "1",
        observations = 0)
    
    n_obs <- bind_rows(n_obs, replace_all_missing)
    
  }
  
  n_obs <- n_obs %>%
    dplyr::filter(levels == 1) %>%
    dplyr::select(-levels)
  
  min_obs <- ds.listDisclosureSettings(datasources = conns)$ds.disclosure.settings %>%
    map_df(~ .$nfilter.subset) %>%
    pivot_longer(
      cols = everything(),
      names_to = "cohort",
      values_to = "min_obs"
    )
  
  disclosure_ref <- left_join(n_obs, min_obs, by = "cohort") %>%
    mutate(
      boole_short = bin_vec,
      enough_obs = ifelse(observations > min_obs, TRUE, FALSE)
    )
  
  return(disclosure_ref)
}

#' Checks the server environment for new objects generated and removes them
#'
#' Often with dsHelper functions we will create temporary objects which we want
#' to remove. This function checks the difference between the output of `ds.ls`
#' called at different points in the analysis. It removes objects present in
#' the second set which were not present in the first.
#'
#' @param start_objs Output from ds.ls run at the start of a function
#' @param others_to_keep Optionally, additional variables not to remove, e.g.
#' you might not want to remove an output object you have created.
#' @template conns
#'
#' @importFrom dsBaseClient ds.ls
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr map pmap imap
#'
#' @noRd
.removeTempObjs <- function(start_objs, others_to_keep, conns) {
  out_to_keep <- NULL
  
  end_objs <- ds.ls(datasources = conns)
  
  remove_ref <- tibble(
    start_clean = start_objs %>% map(~ .x$objects.found),
    end_clean = end_objs %>% map(~ .x$objects.found)
  )
  
  to_keep <- remove_ref %>%
    pmap(function(start_clean, end_clean) {
      start_clean[start_clean %in% end_clean == TRUE]
    })
  
  to_keep <- to_keep %>% map(function(x) {
    c(x, others_to_keep)
  })
  
  to_keep %>%
    imap(
      ~ dh.tidyEnv(obj = .x, type = "keep", conns = conns[.y])
    )
}

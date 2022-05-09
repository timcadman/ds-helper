#' Describes a numeric variable by strata of another numeric grouping variable.
#'
#' This has similar functionality to `tapply` or the dplyr chain `group_by` %>%
#' `summarise`. It offers additional flexilibity over `ds.tapply` in that it
#' allows you to specify upper and lower values for each strata. By contrast,
#' ds.tapply will produce a summary based on every unique value of the grouping
#' variable, which may not always be what is required.
#'
#' @importFrom DSI datashield.connections_find datashield.assign
#' @importFrom dsBaseClient ds.meanSdGp
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows %>% slice
#'
#' @template conns
#' @template df
#' @param outcome String specifying outcome variable within `df`.
#' @param group_var String specifying grouping variable within 'df'.
#' @param intervals Optionally, numeric vector defining how to stratify
#' `group_var`. Values should specify alternately lower and upper values for
#' each strata. If NULL, `outcome` is summarised by every unique value of
#' `group_var`.
#' @template checks
#'
#' @return Tibble containing mean values for each strata of `group_var`.
#'
#' @family descriptive functions
#'
#' @export
dh.meanByGroup <- function(df = NULL, outcome = NULL, group_var = NULL,
                           intervals = NULL, conns = NULL, checks = FALSE) {
  value <- op <- tmp <- varname <- new_df_name <- age <- group <- cohort <-
    . <- enough_obs <- variable <- NULL

  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(outcome)) {
    stop("`outcome` must not be NULL.", call. = FALSE)
  }

  if (is.null(group_var)) {
    stop("`group_var` must not be NULL.", call. = FALSE)
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  if (checks == TRUE) {
    .isDefined(df = df, vars = vars, conns = conns)
  }

  ## There is an easy way and a hard way. If we bin based on integer units of
  ## the binning variable it is quite quick

  if (is.null(intervals)) {


    ## ---- First we round up the age variable -----------------------------------------------
    DSI::datashield.assign(
      conns, "age_tmp", as.symbol(paste0(df, "$", group_var, "+0.5"))
    )

    calltext <- call("asIntegerDS", "age_tmp")
    DSI::datashield.assign(conns, "age_round", calltext)

    ## ---- Now we get the mean values for the outcome by age --------------------------------
    calltext <- paste0(
      "meanSdGpDS(", paste0(df, "$", outcome), ",",
      "age_round", ")"
    )
    
    mean_tmp <- DSI::datashield.aggregate(conns, as.symbol(calltext))
    ## ---- Deal with disclosure issues ----------------------------------------
    warnings <- .checkDisclosureMeanGp(mean_tmp)
    
    if(nrow(warnings$issues) > 0){
      
      warning(
"The following cohorts have at least one group with between 1 and 2 observations 
so it is not possible to return grouped values. Try collapsing some categories or 
re-run the function using the `intervals` argument. \n\n", 
      
paste0(warnings$issues$cohort, collapse = ", ")

)
      
    }
    
    ## ---- Now put into neat long format -------------------------------------------------------
    out <- warnings$no_issues_data %>%
      map(function(x) {
        x$Mean_gp %>%
          as_tibble(rownames = "age") %>%
          mutate(age = as.numeric(str_remove(age, "age_round_")))
      }) %>%
      bind_rows(.id = "cohort") %>%
      dplyr::rename(mean = value)

    ## ---- Remove temporary objects ------------------------------------------------------------
    dh.tidyEnv(
      obj = c("age_tmp", "age_round"),
      type = "remove", 
      conns = conns
    )
  } else if (!is.null(intervals)) {

    ## ---- This is the harder one -------------------------------------------------------------------

    ## First we need to create a table defining our age bands.
    pairs <- split(intervals, ceiling(seq_along(intervals) / 2))

    subnames <- unlist(
      pairs %>% map(~ paste0("grp", "_", paste0(., collapse = "_"))),
      use.names = FALSE
    )

    cats <- tibble(
      varname = rep(subnames, each = 2),
      value = intervals,
      op = rep(c(">=", "<="), times = (length(intervals) / 2)),
      tmp = ifelse(op == ">=", "gte", "lte"),
      new_df_name = paste0("grp", tmp, value)
    )


    ## Now we create vectors of 1s and 0s indicating whether the above criteria
    ## are met for each subject.
    cats %>%
      pmap(function(value, op, new_df_name, ...) {
        ds.Boole(
          V1 = paste0(df, "$", group_var),
          V2 = value,
          Boolean.operator = op,
          newobj = new_df_name,
          datasources = conns
        )
      })

    suppressMessages(
      assign_conditions <- cats %>%
        group_by(varname) %>%
        summarise(condition = paste(new_df_name, collapse = "*"))
    )

    assign_conditions %>%
      pmap(function(condition, varname) {
        DSI::datashield.assign(conns, varname, as.symbol(condition))
      })
    
    ## Check disclosure issues
    disclosure <- assign_conditions$varname %>%
      map(~.checkDisclosure(., conns = conns)) %>%
      set_names(assign_conditions$varname) %>%
      bind_rows(.id = "variable")
    

    no_obs <- disclosure %>% dplyr::filter(enough_obs == FALSE)
    
    if(nrow(no_obs) > 0){
      
      warning(paste(capture.output({
        cat("These cohorts have no observations within the following
              intervals and will be excluded\n\n")
        no_obs %>% dplyr::select(variable, cohort) %>%
          print(n = Inf)
      }), collapse = "\n"))

    }
    
    valid_obs <- disclosure %>% dplyr::filter(enough_obs == TRUE)
    
    ## We then use these vectors to summarise mean observed height at the age
    ## periods we are interested in.
    obs_by_agecat_comb <- valid_obs %>%
      pmap(function(variable, cohort, ...){
        
        ds.meanSdGp(
          x = paste0(df, "$", outcome),
          y = variable,
          type = "split",
          datasources = conns[cohort])
      })
    
    ## Now we take these values and put them into a neater table
    out <- obs_by_agecat_comb %>%
      map(function(x) {
        x$Mean_gp %>%
          as_tibble(rownames = "group") %>%
          slice(2) %>%
      pivot_longer(
        cols = -group,
        names_to = "cohort", 
        values_to = "mean")}) %>%
      bind_rows() %>%
      mutate(group = str_remove(group, "_[^_]+$")) %>%
      mutate(group = str_remove(group, "grp_")) %>%
      dplyr::select(cohort, group, mean)

    ## ---- Remove temporary objects -------------------------------------------
    dh.tidyEnv(
      obj = c(cats$new_df_name, assign_conditions$varname),
      type = "remove", 
      conns = conns
    )
  }

  return(out)
}


#' Internal function to check disclosure issues in this function
#' 
#' @param mean_gp output from ds.meanSdGp
#' 
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr %>% bind_rows filter
#' @noRd
.checkDisclosureMeanGp <- function(mean_gp){
  
  warnings <- mean_gp %>% map(function(x){
    tibble(warning = !is.null(x$Warning))}) %>%
    bind_rows(.id = "cohort")
  
  issues <- warnings %>% dplyr::filter(warning == TRUE)  
  no_issues <- mean_gp[!names(mean_gp) %in% issues$cohort] 
  
  return(list(
    summary = warnings, 
    issues = issues, 
    no_issues_data = no_issues))
}
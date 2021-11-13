#' Derives one or more outcome variable(s) from repeated measures data
#'
#' Many analyses will want to use outcomes derived at a single time point,
#' e.g. BMI between ages 10-14. This function automates the process to do this
#' which is quite complex in DataSHIELD. Note that for big datasets this takes
#' a long time to run.
#'
#' @param conns connections object for DataSHIELD backends
#' @param df opal dataframe
#' @param outcome name of repeated measures outcome variable
#' @param age_var Vector of values indicating pairs of low and high values
#'             for which to derive outcome variables for. Must be even length
#' @param bands vector of alternating lower and upper age bands for variable(s)
#'              you want to create. Variables will be derived for the age range
#'              > lowest value and <= highest value for each band.
#' @param mult_action if a subject has more than one value within the time
#'                    period do we keep the earliest or latest? Default =
#'                    "earliest"
#' @param mult_vals if "mult_action = nearest", this argument specifies which
#'                  which value in each age band to chose values closest to
#'                  in case of multiple values
#' @param keep_original keep original data frame in the DataSHIELD backend
#' @param df_name specify data frame name on the DataSHIELD backend
#' @param id_var specify id variable (default assumes LifeCycle name 'child_id')
#' @param band_action specify how the values of bands are evaluated in making the subsets.
#' "g_l" = greater than the lowest band and less than the highest band;
#' "ge_le" = greater or equal to the lowest band and less than or equal to the highest band;
#' "g_le" = greater than the lowest band and less than or equal to the highest band;
#' "ge_l" = greater than or equal to the lowest band and less than the highest band;
#'
#' @return a dataset containing the newly derived variables
#'
#' @importFrom dsBaseClient ds.colnames ds.asNumeric ds.assign ds.Boole
#'             ds.dataFrame ds.ls ds.make ds.dataFrameSort ds.dataFrameSubset
#'             ds.listDisclosureSettings ds.mean ds.merge ds.reShape ds.isNA
#'             ds.replaceNA
#' @importFrom purrr pmap map_dfr
#' @importFrom tidyr pivot_longer tibble
#' @importFrom dplyr pull %>% rename
#' @importFrom stringr str_extract
#' @importFrom magrittr %<>%
#' @importFrom DSI datashield.connections_find
#' @importFrom rlang :=
#'
#' @export
# nolint
dh.makeOutcome <- function(df = NULL, outcome = NULL, age_var = NULL, bands = NULL, mult_action = NULL, # nolint
                           mult_vals = NULL, keep_original = FALSE, df_name = NULL, conns = NULL, id_var = "child_id",
                           band_action = NULL) {

  
  op <- tmp <- dfs <- new_subset_name <- value <- cohort <- varname <- new_df_name <-
  available <- bmi_to_subset <- ref_val <- . <- NULL
  
  start_objs <- ds.ls(datasources = conns) 

  cat("This may take some time depending on the number and size of datasets\n\n")

  message("** Step 1 of 9: Checking input data ... ", appendLF = FALSE)

.checkInputs()

available_outcome <- .checkDataAvailable(
  df = df,
  var = outcome,
  conns = conns) 

available_age <- .checkDataAvailable(
  df = df,
  var = age_var,
  conns = conns)

available <- left_join(
  x = available_outcome,
  y = available_age,
  by = "cohort") %>%
dplyr::filter(if_all(-cohort, ~ .x == FALSE))

valid_coh <- available$cohort

message("DONE", appendLF = TRUE)

message("** Step 2 of 9: Preparing data ... ", appendLF = FALSE)

.makeSlim()

message("DONE", appendLF = TRUE)

message("** Step 3 of 9: Defining subsets ... ", appendLF = FALSE)

pairs <- split(bands, ceiling(seq_along(bands) / 2))

op_symbol <- case_when(
  band_action == "g_l" ~ c(">", "<"),
  band_action == "ge_le" ~ c(">=", "<="),
  band_action == "g_le" ~ c(">", "<="),
  band_action == "ge_l" ~ c(">=", "<")
)

subset_ref <- tibble(
  value_1 = bands[c(TRUE, FALSE)], 
  op_1 = op_symbol[1],
  value_2 = bands[c(FALSE, TRUE)], 
  op_2 = op_symbol[2],
  boole_name = pairs %>% map_chr(~ paste0("boole", "_", paste0(., collapse = "_"))),
  subset_name = pairs %>% map_chr(~ paste0("subset", "_", paste0(., collapse = "_")))
)

subset_ref %>%
pmap(function(value_1, op_1, value_2, op_2, boole_name, ...){

.BooleTwoConditions(
  df = "df_slim", 
  var = age_var, 
  value_1 = value_1, 
  op_1 = op_1, 
  value_2 = value_2, 
  op_2 = op_2, 
  newobj = boole_name, 
  conns = conns[valid_coh])
})

message("DONE", appendLF = TRUE)

message("** Step 4 of 9: Check for disclosure issues ... ", appendLF = FALSE)

# We need to check that the subsets will have enough rows to avoid triggering
# disclosure traps.
discloure_ref <- subset_ref$boole_name %>%
map(
  ~.checkDisclosure(
    bin_vec = .x,
    conns = conns[valid_coh]
  )) %>%
bind_rows

if (nrow(discloure_ref) < 1) {
    stop("No subsets can be created as they would all contain fewer rows than the disclosure filter value")

  }

failed_disclosure <- discloure_ref %>%
left_join(., subset_ref, by = "boole_name") %>%
dplyr::filter(enough_obs == FALSE)

  if (nrow(failed_disclosure) > 1) {
    warning("The following subsets cannot be created as they would contain fewer observations
      than the disclosure filter value: \n\n", 
    paste0(failed_disclosure$cohort, ": " ,failed_disclosure$subset_name, sep = "\n")
  )
  }

  subset_ref_final <- left_join(subset_ref, discloure_ref, by = "boole_name") %>%
  dplyr::filter(enough_obs == TRUE) %>%
  select(cohort, boole_name, subset_name)

message("DONE", appendLF = TRUE)

message("** Step 5 of 9: Creating subsets ... ", appendLF = FALSE)

  subset_ref_final %>%
    pmap(
      function(cohort, boole_name, subset_name) {
        ds.dataFrameSubset(
          df.name = "df_slim",
          V1.name = boole_name,
          V2.name = "1",
          Boolean.operator = "==",
          keep.NAs = FALSE,
          newobj = subset_name,
          datasources = conns[cohort]
        )
      }
    )

  message("DONE", appendLF = TRUE)

  message("** Step 6 of 9: Dealing with subjects with multiple observations within age bands ... ",
    appendLF = FALSE
  )

  if (mult_action == "nearest") {

nearest_ref <- tibble(
  subset_name = unique(subset_ref_final$subset_name),
  nearest_value = mult_vals
)

sort_ref <- left_join(subset_ref_final, nearest_ref, by = "subset_name")

} else if (mult_action %in% c("earliest", "latest")) {

sort_ref <- subset_ref_final

}

sort_ref <- sort_ref %>%
mutate(sort_name = paste0(subset_name, "_s"))

sort_ref %>%
pmap(function(cohort, subset_name, sort_name, ...){

.sortSubset(
  mult_action = mult_action,
  subset_name = subset_name,
  age_var = age_var,
  newobj = sort_name,
  conns = conns[cohort])

})

message("DONE", appendLF = TRUE)

message("** Step 7 of 9: Reshaping to wide format ... ", appendLF = FALSE)

  reshape_ref <- sort_ref %>%
    mutate(
      suffix = str_extract(subset_name, "[^_]+$"),
      wide_name = paste0(subset_name, "_w")
    )

reshape_ref %>%
pmap(function(cohort, sort_name, suffix, wide_name, ...){

.reshapeSubset(
  sorted_subset = sort_name, 
  var_suffix = suffix, 
  conns = conns[cohort], 
  newobj = wide_name)

})

message("DONE", appendLF = TRUE)


  ## ---- Merge back with non-repeated dataset -----------------------------------
  message("** Step 6 of 7: Creating final dataset ... ", appendLF = FALSE)

  suppressMessages(
    made_vars <- cats_to_subset %>%
      arrange(cohort) %>%
      group_by(cohort) %>%
      summarise(subs = paste(varname, collapse = ",")) %>%
      map(~ strsplit(., ","))
  )

  finalvars <- made_vars$sub %>% map(~ paste0(., "_wide"))

  names(finalvars) <- unlist(made_vars$cohort)


  if (is.null(df_name)) {
    out_name <- paste0(outcome, "_", "derived")
  } else {
    out_name <- df_name
  }

  finalvars %>%
    imap(function(.x, .y) {
      if (length(.x) == 1) {
        ds.dataFrame(
          x = .x,
          newobj = out_name,
          datasources = conns[.y]
        )
      }

      if (length(.x) == 2) {
        ds.merge(
          x.name = .x[[1]],
          y.name = .x[[2]],
          by.x.names = id_var,
          by.y.names = id_var,
          all.x = TRUE,
          all.y = TRUE,
          newobj = out_name,
          datasources = conns[.y]
        )
      }

      if (length(.x) > 2) {
        ds.merge(
          x.name = .x[[1]],
          y.name = .x[[2]],
          by.x.names = id_var,
          by.y.names = id_var,
          all.x = TRUE,
          all.y = TRUE,
          newobj = out_name,
          datasources = conns[.y]
        )

        remaining <- tibble(
          dfs = .x[3:length(.x)],
          cohort = rep(.y, length(dfs))
        )

        remaining %>%
          pmap(function(dfs, cohort) {
            ds.merge(
              x.name = out_name,
              y.name = dfs,
              by.x.names = id_var,
              by.y.names = id_var,
              all.x = TRUE,
              all.y = TRUE,
              newobj = out_name,
              datasources = conns[cohort]
            )
          })
      }
    })

  if (keep_original == TRUE) {
    ds.merge(
      x.name = out_name,
      y.name = df,
      by.x.names = id_var,
      by.y.names = id_var,
      all.x = TRUE,
      all.y = TRUE,
      newobj = out_name,
      datasources = conns[valid_coh]
    )
  }

  message("DONE", appendLF = TRUE)


  ## ---- Tidy environment -------------------------------------------------------
  message("** Step 7 of 7: Removing temporary objects ... ", appendLF = FALSE)

  end_objs <- ds.ls(datasources = conns)

  to_keep <- list(
    before = start_objs %>% map(function(x) {
      x$objects.found
    }),
    after = end_objs %>% map(function(x) {
      x$objects.found
    })
  ) %>%
    pmap(function(before, after) {
      before[before %in% after == TRUE]
    })

  ## but we keep the final dataset
  to_keep <- to_keep %>% map(function(x) {
    c(x, out_name)
  })

  to_keep %>%
    imap(
      ~ dh.tidyEnv(obj = .x, type = "keep", conns = conns[.y])
    )

  ##  Remove temporary column created whilst making df.
  tmp_to_rem <- ds.colnames(out_name, datasources = conns[valid_coh]) %>%
    map(function(x) {
      which(str_detect(x, "outcome_comp") == FALSE)
    })

  ds.length(paste0(out_name, "$", id_var), type = "split", datasources = conns[valid_coh]) %>%
    setNames(names(conns)) %>%
    imap(
      ~ ds.rep(
        x1 = 1,
        times = .x,
        source.times = "c",
        each = 1,
        source.each = "c",
        newobj = "tmp_id",
        datasources = conns[.y]
      )
    )

  tmp_to_rem %>%
    imap(
      ~ ds.dataFrameSubset(
        df.name = out_name,
        V1.name = "tmp_id",
        V2.name = "1",
        Boolean.operator = "==",
        keep.cols = .x,
        keep.NAs = TRUE,
        newobj = out_name,
        datasources = conns[.y]
      )
    )

  message("DONE", appendLF = TRUE)

  cat(
    "\nDataframe", "'", out_name, "'",
    "created containing the following variables:\n\n"
  )

  print(data_available)

  cat("\nUse 'dh.getStats' to check (i) that all values are plausible, and (ii)
that the 5th and 95th percentiles fall within the specified upper and lower
bands. Unfortunately you can't check min and max values due to disclosure
restrictions.\n\n")
}

#' Perform various checks on the availability and class of input objects
#'
#' @importFrom DSI datashield.connections_find datashield.aggregate
#' @importFrom rlang arg_match
#'
#' noRD
.checkInputs <- function(){

if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(outcome)) {
    stop("Please specify an outcome variable")
  }

  if (is.null(age_var)) {
    stop("Please specify an age variable")
  }

  if (is.null(bands)) {
    stop("Please specify age bands which will be used to create the subset(s)")
  }

  if (is.null(band_action)) {
    stop("Please specify how you want to evaluate the age bands using argument 'band_action'")
  }

  if (is.null(mult_action)) {
    stop("Please specify how you want to deal with multiple observations within an age
         bracket using the argument 'mult_action")
  }

  if ((length(bands) %% 2 == 0) == FALSE) {
    stop("The length of the vector provided to the 'bands' argument is not an even number",
      call. = FALSE
    )
  }

  mult_action <- arg_match(mult_action, c("earliest", "latest", "nearest"))
  band_action <- arg_match(band_action, c("g_l", "ge_le", "g_le", "ge_l"))

if(is.null(mult_vals) & (length(mult_vals) != length(bands) /2 )){

stop("Length of argument 'mult_vals' must be half the length of argument 'bands'")

}

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doVarsExist(df = df, vars = outcome, conns = conns)
  dh.doesDfExist(df = df, conns = conns)


  cally <- call('classDS', paste0(df, "$", outcome))
  outcome_class <- DSI::datashield.aggregate(conns, cally)

  if (length(unique(outcome_class)) > 1) {
    stop("The outcome variable does not have the same class in all studies")
  } else if (any(!outcome_class %in% c("numeric", "integer"))) {
    stop("The class of the outcome variable needs to be either numeric or integer.")
  }

  cally <- call('classDS', paste0(df, "$", age_var))
  age_var_class <- DSI::datashield.aggregate(conns, cally)

 if (length(unique(age_var_class)) > 1) {
    stop("The age variable does not have the same class in all studies.")
  } else if (any(!age_var_class %in% c("numeric", "integer"))) {
    stop("The class of the age variable needs to be either numeric or integer.")
  }

}

#' Check that there is some non-missing data on provided variable.
#' This is needed so we don't try to create empty subsets later
#'
#' @param df 
#' @param var variable in df to check
#' @param conns datashield connections object
#'
#' @importFrom dsBaseClient ds.isNA
#' @importFrom dplyr %>% bind_rows filter pull
#' @importFrom tidyr pivot_longer
#'
#' @noRD
.checkDataAvailable <- function(df, var, conns){

missing_col_name <- paste0(var, "_missing")

check_missing <- ds.isNA(
  x = paste0(df, "$", var), 
  datasources = conns) %>%
bind_rows %>%
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
      "and will be excluded from the analysis")

  }

  return(check_missing)
}

#' Trim the dataset to include only relevant columns and non-
#' missing rows. This should improve performance with large
#' datasets
#'
#' @importFrom dsBaseClient ds.dataFrame
#' 
#' noRD
.makeSlim <- function(){
  calltext <- call("asNumericDS", paste0(df, "$", age_var))
  DSI::datashield.assign(conns[valid_coh], "age", calltext)

dh.dropCols(
  df = df, 
  vars = c(id_var, outcome), 
  new_df_name = "df_slim",
  type = "keep")

  ds.dataFrame(
    x = c("df_slim", "age"),
    newobj = "df_slim",
    datasources = conns[valid_coh], 
    check.rows = FALSE,
    check.names = FALSE,
    completeCases = TRUE
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
#' NoRD

df = df
var = outcome
op_1 = ">="
op_2 = "<="
newobj = "test"
value_1 = 3
value_2 = 5


.BooleTwoConditions <- function(df, var, value_1, op_1, value_2, op_2, newobj, conns){

op_ref <- tibble(
  symbol = c("==", "!=", "<", "<=", ">", ">="),
  number = seq(1, 6, 1)
)

get_op_number <- function(op){

op_ref %>%
dplyr::filter(symbol == op) %>% 
dplyr::select(number) %>% 
pull

}

op_1 <- get_op_number(op_1)
op_2 <- get_op_number(op_2)

calltext <- call("BooleDS", paste0(df, "$", var), value_1, op_1, "NA", TRUE)
DSI::datashield.assign(conns, "boole_1", calltext)

calltext <- call("BooleDS", paste0(df, "$", var), value_2, op_2, "NA", TRUE)
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
#' @importFrom ready pivot_longer
#' @importFrom tibble as_tibble
.checkDisclosure <- function(bin_vec, conns){

n_obs <- ds.table(bin_vec)$output.list$TABLE_rvar.by.study_counts %>%
 as_tibble(rownames = "levels") %>%
 dplyr::filter(levels == 1) %>%
    pivot_longer(
      cols = c(-levels),
      names_to = "cohort", 
      values_to = "observations"
    ) %>%
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
  boole_name = bin_vec,
  enough_obs = ifelse(observations > min_obs, TRUE, FALSE))

return(disclosure_ref)

} 

#' Sorts the subsets. This is necessary because it determines how multiple rows
#' per id are handled when reshaping to wide format
#' 
#' @param mult_action how to handle multiple observations.
#' @param mult_values values when mult_action == "nearest"
#' @param subset_name name of subset to sort
#' @param age_var name of age var in subset
#' @param newobj name for sorted dataset
#' @param conns datashield connections object
#' 
#' @importFrom DSI datashield.assign
#' @importFrom dsBaseClient ds.dataFrameSort
#' 
#' @return noRD
.sortSubset <- function(mult_action, mult_vals, subset_name, age_var, newobj, conns){

if(mult_action == "nearest"){

    ## Make a variable specifying distance between age of measurement and prefered
    ## value (provided by "mult_vals")

calltext <- paste0(
          "((", subset_name, "$", "age", "-", nearest_value, ")", "^2",
          ")", "^0.5"
        )

DSI::datashield.assign(conns, "difference_val", as.symbol(calltext))

sort_key <- "difference_val"
sort_action <- FALSE

} else if (mult_action %in% c("earliest", "latest")){

sort_key <- paste0(subset_name, "$", age_var)
sort_action <- ifelse(mult_action == "earliest", FALSE, TRUE)

}

ds.dataFrameSort(
  df.name = subset_name,
  sort.key.name = sort_key,
  newobj = newobj,
  sort.descending = sort_action,
  datasources = conns)

}

#' Reshapes sorted subset to wide format
#'
#' @param sorted_subset name of sorted subset created by function .sortSubset
#' @param var_suffix integer which will form the suffix of outcome and age variables in long format
#' @param conns datashield connections object
#' @param newobj name for created wide subset'
#'
#' NoRD
.reshapeSubset <- function(sorted_subset, var_suffix, conns, newobj){

# We need a vector the length of our subset with an integer value describing 
# the name of the subset. We use this to create our final variables names

calltext <- paste0("(", sorted_subset, "$", age_var, "*0)+", value)
DSI::datashield.assign(conns, "variable_suffix", as.symbol(calltext))

ds.dataFrame(
  x = c(sorted_subset, "variable_suffix"),
  newobj = "subset_w_suffix",
  datasources = conns)

# Now we convert to wide format
ds.reShape(
  data.name = "subset_w_suffix",
  timevar.name = "variable_suffix",
  idvar.name = id_var,
  v.names = c(outcome, age_var),
  direction = "wide",
  newobj = newobj,
  datasources = conns)

}




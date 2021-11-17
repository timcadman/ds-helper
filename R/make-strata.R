#' Creates strata of a repeated measures variable within specified age or time bands 
#'
#' For many analyses you will want to use values from repeated measures data within
#' specified bands. For example, you may have BMI measures between ages 0-18, but want
#' to create a variable for each subject which is their BMI between ages 9-11.
#' This is quite complicated to do in DataSHIELD so this function automates the process.
#'
#' The steps here are equivalent to the following dplyr chain:
#' 
#' df %>% 
#' group_by(band, id) %>% 
#' arrange() %<% 
#' slice(1) 
#' 
#'
#' One of the complexities of this operation is how to deal with cases where 
#' subjects have multiple observations within a specified band. This is handled
#' by first sorting the group so that the required value is first. When we reshape
#' the data all but the first value for subjects with multiple observations within a band
#' are dropped.
#'
#' Note that for big datasets this will take a long time to run.
#'
#' @param df String providing name of opal/armadillo dataframe in long format.
#' @param id_var String providing name of subject id variable in df.
#' @param var_to_subset String providing name of the variable in df
#'                      to stratify according to bands.
#' @param age_var String providing name of age or time variable in df.
#' @param bands A numeric vector of alternating lower and upper values to specify 
#'              the bands in which to derive strata of var_to_subset. This vector should
#'              be an even number and twice the length of the number of bands required.
#' @param band_action String specifying how the values provided in 'bands' arguments are 
#'                    evaluated in creating the strata:
#' "g_l" = greater than the lowest band and less than the highest band;
#' "ge_le" = greater or equal to the lowest band and less than or equal to the highest band;
#' "g_le" = greater than the lowest band and less than or equal to the highest band;
#' "ge_l" = greater than or equal to the lowest band and less than the highest band;
#' @param mult_action String specifying how to handle cases where a subject has more 
#'                    than one measurement within a specified band. Use 'earliest' to
#'                    take the earliest measurement, 'latest' to take the
#'                    latest measurement and 'nearest' to take the measurement nearest to 
#'                    the value(s) specified in mult_vals.
#' @param mult_vals Numeric vector specifying the value in each age band to chose values 
#'                  closest to. Required only if mult_action = "nearest". The order and length of the vector
#'                  should correspond to the order and number of the bands.
#' @param df_name String providing name of data frame to be created on the DataSHIELD backend
#' @param conns Connections object for DataSHIELD backends.
#'
#' @return A serverside dataframe in wide format containing the newly derived variables. For each band specified two
#'         variables will be returned: (i) var_to_subset and (ii) age_var. The suffix
#'          .[lower_band] identifies the band for that variable. 
#'
#' @importFrom dsBaseClient ds.colnames ds.asNumeric ds.assign ds.Boole
#'             ds.dataFrame ds.ls ds.make ds.dataFrameSort ds.dataFrameSubset
#'             ds.listDisclosureSettings ds.mean ds.merge ds.reShape ds.isNA
#'             ds.replaceNA
#' @importFrom purrr pmap map_dfr
#' @importFrom tidyr pivot_longer tibble
#' @importFrom dplyr pull %>% rename if_all
#' @importFrom stringr str_extract
#' @importFrom magrittr %<>%
#' @importFrom DSI datashield.connections_find
#' @importFrom rlang :=
#'
#' @export
# nolint
dh.makeStrata <- function(df = NULL, id_var = NULL, age_var = NULL, var_to_subset = NULL, bands = NULL, mult_action = NULL, # nolint
                        mult_vals = NULL, df_name = NULL, conns = NULL, band_action = NULL) {
  op <- tmp <- dfs <- new_subset_name <- value <- cohort <- varname <- new_df_name <-
    available <- bmi_to_subset <- ref_val <- enough_obs <- boole_name <- subset_name <- wide_name <-
    end_objs <- . <- nearest_value <- age <- NULL

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  start_objs <- ds.ls(datasources = conns)

  cat("This may take some time depending on the number and size of datasets\n\n")

  message("** Step 1 of 9: Checking input data ... ", appendLF = FALSE)

  .checkInputs(
    df = df, 
    var_to_subset = var_to_subset, 
    age_var = age_var, 
    bands = bands, 
    band_action = band_action,
    mult_action = mult_action,
    mult_vals = mult_vals, 
    conns = conns)

  available_var <- .checkDataAvailable(
    df = df,
    var = var_to_subset,
    conns = conns
  )

  available_age <- .checkDataAvailable(
    df = df,
    var = age_var,
    conns = conns
  )

  available <- left_join(
    x = available_var,
    y = available_age,
    by = "cohort"
  ) %>%
    dplyr::filter(if_all(-cohort, ~ .x == FALSE))

  valid_coh <- available$cohort

  message("DONE", appendLF = TRUE)

  message("** Step 2 of 9: Preparing data ... ", appendLF = FALSE)

  .makeSlim(
    df = df,
    id_var = id_var,
    age_var = age_var,
    var_to_subset = var_to_subset,
    conns = conns[valid_coh])

  message("DONE", appendLF = TRUE)

  message("** Step 3 of 9: Defining subsets ... ", appendLF = FALSE)

  pairs <- split(bands, ceiling(seq_along(bands) / 2))

  op_symbol <- case_when(
    band_action == "g_l" ~ c(">", "<"),
    band_action == "ge_le" ~ c(">=", "<="),
    band_action == "g_le" ~ c(">", "<="),
    band_action == "ge_l" ~ c(">=", "<")
  )

  boole_ref <- tibble(
    value_1 = bands[c(TRUE, FALSE)],
    op_1 = op_symbol[1],
    value_2 = bands[c(FALSE, TRUE)],
    op_2 = op_symbol[2],
    boole_name = pairs %>% map_chr(~ paste0("boole", "_", paste0(., collapse = "_"))),
    subset_name = pairs %>% map_chr(~ paste0("subset", "_", paste0(., collapse = "_")))
  )

  boole_ref %>%
    pmap(function(value_1, op_1, value_2, op_2, boole_name, ...) {
      .BooleTwoConditions(
        df = "df_slim",
        var = age_var,
        value_1 = value_1,
        op_1 = op_1,
        value_2 = value_2,
        op_2 = op_2,
        newobj = boole_name,
        conns = conns[valid_coh]
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 4 of 9: Check for disclosure issues ... ", appendLF = FALSE)

  # We need to check that the subsets will have enough rows to avoid triggering
  # disclosure traps.
  discloure_ref <- boole_ref$boole_name %>%
    map(
      ~ .checkDisclosure(
        bin_vec = .x,
        conns = conns[valid_coh]
      )
    ) %>%
    bind_rows()

  if (nrow(discloure_ref) < 1) {
    stop("No subsets can be created as they would all contain fewer rows than the disclosure filter value")
  }

  failed_disclosure <- discloure_ref %>%
    left_join(., boole_ref, by = "boole_name") %>%
    dplyr::filter(enough_obs == FALSE)

  if (nrow(failed_disclosure) > 1) {
    warning(
      "The following subsets cannot be created as they would contain fewer observations
      than the disclosure filter value: \n\n",
      paste0(failed_disclosure$cohort, ": ", failed_disclosure$subset_name, sep = "\n")
    )
  }

  message("DONE", appendLF = TRUE)

  message("** Step 5 of 9: Creating subsets ... ", appendLF = FALSE)

  subset_ref <- left_join(boole_ref, discloure_ref, by = "boole_name") %>%
    dplyr::filter(enough_obs == TRUE) %>%
    select(cohort, boole_name, subset_name)

  subset_ref %>%
    pmap(
      function(cohort, boole_name, subset_name) {
        ds.dataFrameSubset(
          df.name = "df_slim",
          V1.name = boole_name,
          V2.name = "1",
          Boolean.operator = "==",
          keep.NAs = TRUE,
          newobj = subset_name,
          datasources = conns[cohort]
        )
      }
    )

  message("DONE", appendLF = TRUE)

  message("** Step 6 of 9: Dealing with subjects with multiple observations within age bands ... ",
    appendLF = FALSE
  )

  # We deal with multiple obsevations per subject with a bit of a hack. When we reshape to wide,
  # only the first observation for subject is kept. Here we sort the dataframe to ensure that the
  # first observation is the one that we want to keep, based on the value of "mult_action".

  if (mult_action == "nearest") {
    nearest_ref <- tibble(
      subset_name = unique(subset_ref$subset_name),
      nearest_value = mult_vals
    )

    sort_ref <- left_join(subset_ref, nearest_ref, by = "subset_name")

  } else if (mult_action %in% c("earliest", "latest")) {
    sort_ref <- subset_ref %>%
    mutate(nearest_value = NA)
  }

  sort_ref <- sort_ref %>%
    mutate(sort_name = paste0(subset_name, "_s"))

  sort_ref %>%
    pmap(function(cohort, subset_name, sort_name, nearest_value, ...) {
      .sortSubset(
        mult_action = mult_action,
        nearest_value = nearest_value,
        subset_name = subset_name,
        age_var = age_var,
        newobj = sort_name,
        conns = conns[cohort]
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 7 of 9: Reshaping to wide format ... ", appendLF = FALSE)

  reshape_ref <- sort_ref %>%
    mutate(
      suffix = str_extract(subset_name, "[^_]+$"),
      wide_name = paste0(subset_name, "_w")
    )

  reshape_ref %>%
    pmap(function(cohort, sort_name, suffix, wide_name, ...) {
      .reshapeSubset(
        id_var = id_var, 
        age_var = age_var, 
        var_to_subset = var_to_subset, 
        sorted_subset = sort_name,
        var_suffix = suffix,
        conns = conns[cohort],
        newobj = wide_name
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 8 of 9: Creating final dataset ... ", appendLF = FALSE)

  # Let's make sure we return the correct number of rows. We can use the original
  # dataset and merge back into that.

  .makeEmptyWide(
    df = df,
    id_var = id_var,
    var_to_subset = var_to_subset, 
    conns = conns[valid_coh], 
    finalobj = df_name)

  merge_ref <- reshape_ref %>%
    dplyr::select(cohort, wide_name)

  merge_ref %>%
    pmap(function(cohort, wide_name) {
      ds.merge(
        x.name = df_name,
        y.name = wide_name,
        by.x.names = id_var,
        by.y.names = id_var,
        all.x = TRUE,
        all.y = FALSE,
        newobj = df_name,
        datasources = conns[cohort]
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 7 of 7: Removing temporary objects ... ", appendLF = FALSE)

  .removeTempObjs(
    start_objs = start_objs, 
    others_to_keep = df_name, 
    conns = conns)

  created <- subset_ref %>%
    mutate(age = subset_name %>% str_remove("subset_")) %>%
    dplyr::select(cohort, age)

  message("DONE", appendLF = TRUE)

  cat(
    "\nDataframe ", "'", df_name, "'",
    " has been created containing ", "'", var_to_subset, "'", " variables derived at the following ages:\n\n", 
    sep = ""
  )

  print(created)
}

#' Perform various checks on the availability and class of input objects
#'
#' @importFrom DSI datashield.connections_find datashield.aggregate
#' @importFrom rlang arg_match
#'
#' @noRd
.checkInputs <- function(df, var_to_subset, age_var, bands, band_action, mult_action, mult_vals, conns) {

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(var_to_subset)) {
    stop("Please specify an variable to create subsets for")
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

  if(mult_action == "nearest" & is.null(mult_vals)){

stop("You must provide value(s) to argument mult_vals when mult_action is set to 'nearest'")

  }

  mult_action <- arg_match(mult_action, c("earliest", "latest", "nearest"))
  band_action <- arg_match(band_action, c("g_l", "ge_le", "g_le", "ge_l"))

  if (mult_action == "nearest" & (length(mult_vals) != length(bands) / 2)) {
    stop("Length of argument 'mult_vals' must be half the length of argument 'bands'")
  }

  dh.doVarsExist(df = df, vars = var_to_subset, conns = conns)
  dh.doesDfExist(df = df, conns = conns)


  cally <- call("classDS", paste0(df, "$", var_to_subset))
  var_class <- DSI::datashield.aggregate(conns, cally)

  if (length(unique(var_class)) > 1) {
    stop("The variable to subset does not have the same class in all studies")
  } else if (any(!var_class %in% c("numeric", "integer"))) {
    stop("The class of the variable to subset needs to be either numeric or integer.")
  }

  cally <- call("classDS", paste0(df, "$", age_var))
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

#' Trim the dataset to include only relevant columns and non-
#' missing rows. This should improve performance with large
#' datasets
#'
#' @param df Opal/armadillo data frame
#' @param id_var subject idvariable in df
#' @param age_var subject age variable in df
#' @param var_to_subset variable in df
#' @param conns datashield connection object
#'
#'
#' @importFrom dsBaseClient ds.completeCases
#'
#' @noRd
.makeSlim <- function(df, id_var, age_var, var_to_subset, conns) {
 
  dh.dropCols(
    df = df,
    vars = c(id_var, age_var, var_to_subset),
    new_df_name = "df_slim",
    type = "keep",
    conns = conns
  )

  ds.completeCases(
    x1 = "df_slim",
    newobj = "df_slim",
    datasources = conns
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

  n_obs <- ds.table(bin_vec, datasources = conns)$output.list$TABLE_rvar.by.study_counts %>%
    as_tibble(rownames = "levels") %>%
    dplyr::filter(levels == 1) %>%
    pivot_longer(
      cols = c(-levels),
      names_to = "cohort",
      values_to = "observations"
    ) %>%
    dplyr::select(-levels) %>%
    mutate(cohort = names(conns))

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
      enough_obs = ifelse(observations > min_obs, TRUE, FALSE)
    )

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
#' @noRd
.sortSubset <- function(mult_action, nearest_value, subset_name, age_var, newobj, conns) {
 
  if (mult_action == "nearest") {

    ## Make a variable specifying distance between age of measurement and prefered
    ## value (provided by "mult_vals")

    calltext <- paste0(
      "((", subset_name, "$", age_var, "-", nearest_value, ")", "^2",
      ")", "^0.5"
    )

    DSI::datashield.assign(conns, "difference_val", as.symbol(calltext))

    sort_key <- "difference_val"
    sort_action <- FALSE
  } else if (mult_action %in% c("earliest", "latest")) {
    sort_key <- paste0(subset_name, "$", age_var)
    sort_action <- ifelse(mult_action == "earliest", FALSE, TRUE)
  }

  ds.dataFrameSort(
    df.name = subset_name,
    sort.key.name = sort_key,
    newobj = newobj,
    sort.descending = sort_action,
    datasources = conns
  )
}

#' Reshapes sorted subset to wide format
#'
#' @param sorted_subset name of sorted subset created by function .sortSubset
#' @param id_var subject id variable in sorted subset
#' @param age_var subject age variable in sorted subset
#' @param var_to_subset subject outcome variable in sorted subset
#' @param var_suffix integer which will form the suffix of outcome and age variables in long format
#' @param conns datashield connections object
#' @param newobj name for created wide subset'
#'
#' @noRd
.reshapeSubset <- function(sorted_subset, id_var, age_var, var_to_subset, var_suffix, conns, newobj) {

  # We need a vector the length of our subset with an integer value describing
  # the name of the subset. We use this to create our final variables names

  calltext <- paste0("(", sorted_subset, "$", age_var, "*0)+", var_suffix)
  DSI::datashield.assign(conns, "variable_suffix", as.symbol(calltext))

  ds.dataFrame(
    x = c(sorted_subset, "variable_suffix"),
    newobj = "subset_w_suffix",
    datasources = conns
  )

  # Now we convert to wide format
  ds.reShape(
    data.name = "subset_w_suffix",
    timevar.name = "variable_suffix",
    idvar.name = id_var,
    v.names = c(var_to_subset, age_var),
    direction = "wide",
    newobj = newobj,
    datasources = conns
  )
}


#' We want to return final dataframes with length equal to number of
#' unique subjects in long format. This creates a wide format data
#' frame from the long format input containing only the id variable.
#' 
#' @param df opal/armadillo dataframe
#' @param id_var subject id variable in df
#' @param var_to_subset subject outcome variable in df
#' @param conns datashield connections object
#' @param finalobj name for this data frame
#'
#' @importFrom dsBaseClient ds.dim ds.rep ds.dataFrame
#' @importFrom tibble tibble
#' @importFrom stringr str_remove
#' @importFrom purrr map_int pmap
#'
#' @noRd
.makeEmptyWide <- function(df, id_var, var_to_subset, conns, finalobj) {
 
  df_dim <- ds.dim(df, type = "split", datasources = conns)

  rep_ref <- tibble(
    cohort = str_remove(names(df_dim), "dimensions of data in "),
    length = map_int(df_dim, ~ .x[[1]])
  )

  rep_ref %>%
    pmap(function(cohort, length) {
      ds.rep(
        x1 = 10,
        times = length,
        newobj = "rep_vec",
        datasources = conns[cohort]
      )
    })

  ds.dataFrame(
    c(df, "rep_vec"),
    datasources = conns,
    newobj = "df_tmp",
    check.rows = FALSE,
    check.names = FALSE
  )

  ds.reShape(
    data.name = "df_tmp",
    timevar.name = "rep_vec",
    idvar.name = id_var,
    v.names = var_to_subset,
    direction = "wide",
    newobj = "df_minimal",
    datasources = conns
  )

  dh.dropCols(
    df = "df_minimal",
    vars = "id",
    type = "keep",
    new_df_name = finalobj,
    conns = conns
  )
}

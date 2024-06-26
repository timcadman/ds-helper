#' Creates strata of a repeated measures variable within specified age or time
#' bands
#'
#' For many analyses you may want to create strata of repeated measures data
#' within specified bands. For example, you may have BMI measures between ages
#' 0-18, but want to create a variable for each subject which is their BMI
#' between ages 9-11. This function automates this process.
#'
#' The steps here are equivalent to the following dplyr chain:
#'
#' df %>%
#' group_by(band, id) %>%
#' arrange() %<%
#' slice(1)
#'
#' One of the complexities of this operation is how to deal with cases where
#' subjects have multiple observations within a specified band. This is handled
#' by first sorting the group so that the required value is first. When the
#' data is reshaped to wide format all but the first value for subjects with
#' multiple observations within a band are dropped.
#'
#' Note that for big datasets this will take a long time to run.
#'
#' @template df
#' @template id_var
#' @param var_to_subset Character specifying variable in `df` to stratify
#' according to bands.
#' @param age_var Character specifying age or time variable in `df`.
#' @param bands Numeric vector of alternating lower and upper values specifying
#' the bands in which to derive strata of `var_to_subset`. This vector should
#' be an even number and twice the length of the number of bands required.
#' @param band_action Character specifying how the values provided in `bands`
#' are evaluated in creating the strata:
#' * "g_l" = greater than the lowest band and less than the highest band
#' * "ge_le" = greater or equal to the lowest band and less than or equal to the
#' highest band
#' * "g_le" = greater than the lowest band and less than or equal to the highest
#' band
#' * "ge_l" = greater than or equal to the lowest band and less than the highest
#' band
#' @param mult_action Character specifying how to handle cases where a subject
#' has more than one measurement within a specified band. Use "earliest" to
#' take the earliest measurement, "latest" to take the latest measurement and
#' "nearest" to take the measurement nearest to the value(s) specified in
#' `mult_vals`.
#' @param mult_vals Numeric vector specifying the value in each age band to
#' chose values closest to if subjects have more than one value per band.
#' Required only if mult_action is "nearest". The order and length of the vector
#' should correspond to the order and number of the bands.
#' @param keep_vars Optionally, a vector of variable names within df to include
#' within each strata created.
#' @template new_obj
#' @template conns
#' @template checks
#' @param df_name Retired argument name. Please use `new_obj' instead.
#'
#' @return Servside dataframe in wide format containing the derived variables.
#' For each band specified at least two variables will be returned:
#' * var_to_subset
#' * age_var.
#' The suffix .lower_band identifies the band for that variable.
#'
#' If argument `keep_vars` is not NULL, then additional variables will be
#' added to the data frame representing these variables within the strata
#' created.
#'
#' @family data manipulation functions
#'
#' @importFrom dsBaseClient ds.colnames ds.asNumeric ds.assign ds.Boole
#'             ds.dataFrame ds.ls ds.make ds.dataFrameSort ds.dataFrameSubset
#'             ds.listDisclosureSettings ds.mean ds.merge ds.reShape ds.isNA
#'             ds.replaceNA
#' @importFrom purrr pmap map_dfr
#' @importFrom tidyr pivot_longer tibble
#' @importFrom dplyr pull %>% rename all_vars
#' @importFrom stringr str_extract
#' @importFrom magrittr %<>%
#' @importFrom DSI datashield.connections_find
#' @importFrom rlang :=
#'
#' @md
#'
#' @export
dh.makeStrata <- function(df = NULL, id_var = NULL, age_var = NULL, var_to_subset = NULL, bands = NULL, # nolint
                          mult_action = NULL, mult_vals = NULL, keep_vars = NULL, new_obj = NULL,
                          band_action = NULL, conns = NULL, checks = TRUE, df_name = NULL) {
  op <- tmp <- dfs <- new_subset_name <- value <- cohort <- varname <- new_df_name <-
    available <- bmi_to_subset <- ref_val <- enough_obs <- boole_name <- subset_name <- wide_name <-
    end_objs <- . <- nearest_value <- age <- subset_short <- suffix <- value_1 <- value_2 <- Var1 <-
    Var2 <- var <- value <- NULL

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  start_objs <- ds.ls(datasources = conns)

  cat("This may take some time depending on the number and size of datasets\n\n")

  message("** Step 1 of 9: Checking input data ... ", appendLF = FALSE)

  if (checks == TRUE) {
    .checkInputs(
      df = df,
      id_var = id_var,
      var_to_subset = var_to_subset,
      age_var = age_var,
      bands = bands,
      band_action = band_action,
      mult_action = mult_action,
      mult_vals = mult_vals,
      conns = conns,
      new_obj = new_obj,
      df_name = df_name,
      keep_vars = keep_vars
    )
  }

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
    dplyr::filter_at(vars(-cohort), all_vars(. == FALSE))

  valid_coh <- available$cohort

  message("DONE", appendLF = TRUE)

  message("** Step 2 of 9: Preparing data ... ", appendLF = FALSE)

  .makeSlim(
    df = df,
    id_var = id_var,
    age_var = age_var,
    var_to_subset = var_to_subset,
    keep_vars = keep_vars,
    conns = conns[valid_coh]
  )

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
  ) %>%
    mutate(across(
      c(boole_name, subset_name),
      ~ str_replace_all(
        string = .,
        pattern = "-",
        replacement = "m"
      )
    )) %>%
    mutate(
      boole_short = paste0("bl_", seq(1, length(boole_name))),
      subset_short = paste0("sb_", seq(1, length(subset_name)))
    )


  boole_ref %>%
    pmap(function(value_1, op_1, value_2, op_2, boole_short, ...) {
      .BooleTwoConditions(
        df = "df_slim",
        var = age_var,
        value_1 = value_1,
        op_1 = op_1,
        value_2 = value_2,
        op_2 = op_2,
        newobj = boole_short,
        conns = conns[valid_coh]
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 4 of 9: Check for disclosure issues ... ", appendLF = FALSE)

  # We need to check that the subsets will have enough rows to avoid triggering
  # disclosure traps.
  discloure_ref <- boole_ref$boole_short %>%
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
    left_join(., boole_ref, by = "boole_short") %>%
    dplyr::filter(enough_obs == FALSE)

  if (nrow(failed_disclosure) >= 1) {
    warning(
      "The following subsets cannot be created as they would contain fewer observations
      than the disclosure filter value: \n\n",
      paste0(failed_disclosure$cohort, ": ", failed_disclosure$subset_name, sep = "\n")
    )
  }

  message("DONE", appendLF = TRUE)

  message("** Step 5 of 9: Creating subsets ... ", appendLF = FALSE)

  subset_ref <- left_join(boole_ref, discloure_ref, by = "boole_short") %>%
    dplyr::filter(enough_obs == TRUE)

  subset_ref %>%
    pmap(
      function(cohort, boole_short, subset_short, ...) {
        ds.dataFrameSubset(
          df.name = "df_slim",
          V1.name = boole_short,
          V2.name = "1",
          Boolean.operator = "==",
          keep.NAs = TRUE,
          newobj = subset_short,
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
      subset_short = unique(subset_ref$subset_short),
      nearest_value = mult_vals
    )

    sort_ref <- left_join(subset_ref, nearest_ref, by = "subset_short")
  } else if (mult_action %in% c("earliest", "latest")) {
    sort_ref <- subset_ref %>%
      mutate(nearest_value = NA)
  }

  sort_ref <- sort_ref %>%
    mutate(sort_name = paste0(subset_short, "_s"))

  sort_ref %>%
    pmap(function(cohort, subset_short, sort_name, nearest_value, ...) {
      .sortSubset(
        mult_action = mult_action,
        nearest_value = nearest_value,
        subset_name = subset_short,
        age_var = age_var,
        newobj = sort_name,
        conns = conns[cohort]
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 7 of 9: Reshaping to wide format ... ", appendLF = FALSE)

  reshape_ref <- sort_ref %>%
    mutate(
      suffix = str_extract(subset_short, "[^_]+$"),
      wide_name = paste0(subset_short, "_w")
    )

  reshape_ref %>%
    pmap(function(cohort, sort_name, suffix, wide_name, ...) {
      .reshapeSubset(
        id_var = id_var,
        age_var = age_var,
        var_to_subset = var_to_subset,
        sorted_subset = sort_name,
        var_suffix = suffix,
        keep_vars = keep_vars,
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
    finalobj = new_obj
  )

  merge_ref <- reshape_ref %>%
    dplyr::select(cohort, wide_name)

  merge_ref %>%
    pmap(function(cohort, wide_name) {
      ds.merge(
        x.name = new_obj,
        y.name = wide_name,
        by.x.names = id_var,
        by.y.names = id_var,
        all.x = TRUE,
        all.y = FALSE,
        newobj = new_obj,
        datasources = conns[cohort]
      )
    })

  ## The last step is to rename created variables with correct suffix
  suffix_ref <- reshape_ref %>%
    dplyr::select(cohort, suffix, value_1, value_2) %>%
    mutate(suffix = paste0(".", suffix))

  var_ref <- c(var_to_subset, age_var, keep_vars)

  rename_ref_coh <- suffix_ref %>%
    group_by(cohort)

  tmp_names <- group_keys(rename_ref_coh) %>%
    unlist()

  rename_ref <- rename_ref_coh %>%
    group_split() %>%
    map(~ expand.grid(.$suffix, var_ref)) %>%
    set_names(tmp_names) %>%
    bind_rows(.id = "cohort") %>%
    dplyr::rename(suffix = Var1, var = Var2) %>%
    left_join(., suffix_ref, by = c("cohort", "suffix")) %>%
    mutate(
      old_name = paste0(var, suffix),
      new_name = paste0(var, ".", value_1, "_", value_2)
    ) %>%
    group_by(cohort)

  rename_ref %>%
    pmap(function(cohort, old_name, new_name, ...) {
      dh.renameVars(
        df = new_obj,
        current_names = old_name,
        new_names = new_name,
        conns = conns[cohort],
        checks = FALSE
      )
    })

  message("DONE", appendLF = TRUE)

  message("** Step 9 of 9: Removing temporary objects ... ", appendLF = FALSE)

  .removeTempObjs(
    start_objs = start_objs,
    others_to_keep = new_obj,
    conns = conns
  )

  created <- rename_ref %>%
    ungroup() %>%
    distinct(cohort, value_1, value_2) %>%
    dplyr::rename(
      lower_band = value_1,
      upper_band = value_2
    ) %>%
    arrange(cohort)

  message("DONE", appendLF = TRUE)

  cat(
    "\nDataframe ", "'", new_obj, "'",
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
.checkInputs <- function(df, id_var, var_to_subset, age_var, bands, band_action, mult_action,
                         mult_vals, conns, new_obj, df_name, keep_vars) {
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }

  if (is.null(var_to_subset)) {
    stop("`var_to_subset` must not be NULL.", call. = FALSE)
  }

  if (is.null(new_obj)) {
    stop("`new_obj` must not be NULL.", call. = FALSE)
  }

  if (is.null(age_var)) {
    stop("`age_var` must not be NULL.", call. = FALSE)
  }

  if (is.null(bands)) {
    stop("`bands` must not be NULL.", call. = FALSE)
  }

  if (is.null(band_action)) {
    stop("`band_action` must not be NULL.", call. = FALSE)
  }

  if (is.null(mult_action)) {
    stop("`mult_action` must not be NULL.", call. = FALSE)
  }

  if (is.null(id_var)) {
    stop("`id_var` must not be NULL.", call. = FALSE)
  }

  if (!is.null(df_name)) {
    warning("Please use `new_obj` instead of `df_name`")
    new_obj <- df_name
  }

  if ((length(bands) %% 2 == 0) == FALSE) {
    stop("The length of the vector specified in `bands` is not an even number.",
      call. = FALSE
    )
  }

  if (mult_action == "nearest" & is.null(mult_vals)) {
    stop("`mult_vals` must not be NULL when `mult_action` is 'nearest'.", call. = FALSE)
  }

  mult_action <- arg_match(mult_action, c("earliest", "latest", "nearest"))
  band_action <- arg_match(band_action, c("g_l", "ge_le", "g_le", "ge_l"))

  if (mult_action == "nearest" & (length(mult_vals) != length(bands) / 2)) {
    stop("Length of `mult_vals` must be half the length of `bands`.", call. = FALSE)
  }

  if (is.null(keep_vars)) {
    .isDefined(df = df, vars = c(id_var, var_to_subset, age_var), conns = conns)
  } else {
    .isDefined(df = df, vars = c(id_var, var_to_subset, age_var, keep_vars), conns = conns)
  }

  cally <- call("classDS", paste0(df, "$", var_to_subset))
  var_class <- DSI::datashield.aggregate(conns, cally)

  if (length(unique(var_class)) > 1) {
    stop("`var_to_subset` does not have the same class in all studies.", call. = FALSE)
  }

  cally <- call("classDS", paste0(df, "$", age_var))
  age_var_class <- DSI::datashield.aggregate(conns, cally)

  if (length(unique(age_var_class)) > 1) {
    stop("`age_var` does not have the same class in all studies.", call. = FALSE)
  } else if (any(!age_var_class %in% c("numeric", "integer"))) {
    stop("`age_var` must be class numeric or integer.", call. = FALSE)
  }
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
.makeSlim <- function(df, id_var, age_var, var_to_subset, conns, keep_vars) {
  vars_to_include <- c(id_var, age_var, var_to_subset)

  if (!is.null(keep_vars)) {
    vars_to_include <- c(vars_to_include, keep_vars)
  }

  dh.dropCols(
    df = df,
    vars = vars_to_include,
    new_obj = "df_slim",
    type = "keep",
    conns = conns,
    checks = FALSE
  )

  dh.defineCases(
    df = df,
    vars = c(age_var, var_to_subset),
    type = "all",
    new_obj = "subset_def",
    conns = conns,
    checks = FALSE
  )

  ds.dataFrameSubset(
    df.name = "df_slim",
    V1.name = "subset_def",
    V2.name = "1",
    Boolean.operator = "==",
    keep.NAs = FALSE,
    newobj = "df_slim",
    datasources = conns
  )
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
.reshapeSubset <- function(sorted_subset, id_var, age_var, var_to_subset, var_suffix, conns, newobj, keep_vars) {
  # We need a vector the length of our subset with an integer value describing
  # the name of the subset. We use this to create our final variables names

  calltext <- paste0("(", sorted_subset, "$", age_var, "*0)+", var_suffix)
  DSI::datashield.assign(conns, "variable_suffix", as.symbol(calltext))

  ds.dataFrame(
    x = c(sorted_subset, "variable_suffix"),
    newobj = "subset_w_suffix",
    datasources = conns,
    stringsAsFactors = FALSE
  )

  vars_to_reshape <- c(var_to_subset, age_var)

  if (!is.null(keep_vars)) {
    vars_to_reshape <- c(vars_to_reshape, keep_vars)
  }

  # Now we convert to wide format
  ds.reShape(
    data.name = "subset_w_suffix",
    timevar.name = "variable_suffix",
    idvar.name = id_var,
    v.names = vars_to_reshape,
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
    cohort = str_remove(names(df_dim), paste0("dimensions of ", df, " in ")),
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
    check.names = FALSE,
    stringsAsFactors = FALSE
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
    vars = id_var,
    type = "keep",
    new_obj = finalobj,
    conns = conns,
    checks = FALSE
  )
}

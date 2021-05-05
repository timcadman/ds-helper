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
#'
#' @return a dataset containing the newly derived variables
#'
#' @importFrom dsBaseClient ds.colnames ds.asNumeric ds.assign ds.Boole
#'             ds.dataFrame ds.ls ds.make ds.dataFrameSort ds.dataFrameSubset
#'             ds.listDisclosureSettings ds.mean ds.merge ds.reShape ds.isNA
#' @importFrom purrr pmap map_dfr
#' @importFrom tidyr pivot_longer tibble
#' @importFrom dplyr pull %>%
#' @importFrom stringr str_extract
#' @importFrom magrittr %<>%
#' @importFrom DSI datashield.connections_find
#'
#' @export
dh.makeOutcome <- function(
                           df = NULL, outcome = NULL, age_var = NULL, bands = NULL, mult_action = c("earliest", "latest", "nearest"),
                           mult_vals = NULL, keep_original = FALSE, df_name = NULL, conns = NULL) {
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

  mult_action <- match.arg(mult_action)

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  op <- tmp <- dfs <- new_subset_name <- value <- cohort <- age <- varname <- new_df_name <- available <- bmi_to_subset <- ref_val <- NULL

  cat("This may take some time depending on the number and size of datasets\n\n")

  message("** Step 1 of 7: Checking input data ... ", appendLF = FALSE)

  ## ---- Store current object names ---------------------------------------------

  start_objs <- ds.ls(datasources = conns)

  ## ---- Argument checks --------------------------------------------------------
  dh.doVarsExist(df = df, vars = outcome, conns = conns)
  dh.doesDfExist(df = df, conns = conns)

  ## ---- Check bands is an even number ------------------------------------------
  if ((length(bands) %% 2 == 0) == FALSE) {
    stop("The length of the vector provided to the 'bands' argument is not an even number",
      call. = FALSE
    )
  }

  ## ---- Check class of outcome -----------------------------------------------
  var_class <- ds.class(datasources = conns, x = paste0(df, "$", outcome))

  if (length(unique(var_class)) > 1) {
    stop("The outcome variable does not have the same class in all studies. 
         Please fix this and run again.")
  } else if (var_class[[1]] == "character") {
    stop("The outcome variable is class 'character'. Please provide either a 
         numeric, integer or factor variable.")
  }

  ## ---- Subset to only include cohorts with some data --------------------------
  ds.asNumeric(datasources = conns, x.name = paste0(df, "$", outcome), newobj = paste0(outcome, "_n"))

  na_replace_vec <- rep("-99999", length(conns))
  
  ds.replaceNA(x=paste0(outcome, "_n"), forNA = na_replace_vec, newobj = "na_replaced", datasources = conns)
  
  ds.Boole(
    V1 = "na_replaced",
    V2 = "-99999",
    Boolean.operator = ">",
    newobj = "outcome_comp",
    datasources = conns
  )
  
  nonmissing <- ds.mean(datasources = conns, x = "outcome_comp")$Mean.by.Study[, "EstimatedMean"] > 0

  if (all(nonmissing == FALSE)) {
    stop("None of the cohorts have available outcome data")
  }

  if (any(nonmissing == FALSE)) {
    warning(paste0(
      paste0(
        "No valid data on ", "'", outcome, "'",
        " available for the following cohort(s): "
      ),
      paste0(names(which(nonmissing == FALSE)), collapse = ", ")
    ), call. = FALSE)
  }

  ## ---- Check there are non missing values for age and outcome ---------------
  age_missing <- unlist(ds.isNA(x = paste0(df, "$", age_var), datasources = conns))
  if (any(age_missing) == TRUE) {
    stop(paste0(
      paste0(
        "No valid data on age of measurement available for the following cohort(s): "
      ),
      paste0(names(which(age_missing == TRUE)), collapse = ", ")
    ), call. = FALSE)
  }

  if (length(nonmissing) == 1) {
    nonmissing <- list(nonmissing)
    names(nonmissing) <- names(conns)
  }

  valid_coh <- names(which(nonmissing == TRUE))


  ## ---- Create numeric version of age_var --------------------------------------
  ds.asNumeric(
    x.name = paste0(df, "$", age_var),
    newobj = "age",
    datasources = conns[valid_coh]
  )

  new_df <- paste0(df, "_tmp")
  ds.dataFrame(
    x = c(df, "age", "outcome_comp"),
    newobj = new_df,
    datasources = conns[valid_coh]
  )

  ## ---- Drop variables we don't need -------------------------------------------
  v_ind <- dh.findVarsIndex(
    conns = conns[valid_coh],
    df = new_df,
    vars = c("child_id", outcome, "age", "outcome_comp")
  )

  ## Now finally we subset based on valid cases and required variables
  v_ind %>%
    imap(
      ~ ds.dataFrameSubset(
        df.name = new_df,
        V1.name = "outcome_comp",
        V2.name = "1",
        Boolean.operator = "==",
        keep.cols = .x,
        keep.NAs = FALSE,
        newobj = new_df,
        datasources = conns[.y]
      )
    )


  ## ---- Make paired list for each band ---------------------------------------
  pairs <- split(bands, ceiling(seq_along(bands) / 2))

  subnames <- unlist(
    pairs %>% map(~ paste0(outcome, "_", paste0(., collapse = "_"))),
    use.names = FALSE
  )

  ## ---- Create table with age bands ------------------------------------------
  cats <- tibble(
    varname = rep(subnames, each = 2),
    value = bands,
    op = rep(c(">", "<="), times = (length(bands) / 2)),
    tmp = ifelse(op == ">", "gt", "lte"),
    new_df_name = paste0(outcome, tmp, value)
  )

  ## ---- Check max character length -------------------------------------------
  if (max(nchar(cats$varname)) + 6 > 20) {
    stop(
      "Due to disclosure settings, the total string length of [outcome] + 
      [max(lower_band)] + [max(upper_band)] + [max(mult_vals)] must be no more 
      than 14 characters. For example: [outcome = 'bmi', max(low_band) = 10, 
      max(upper_band) = 40, max(mult_vals) = 35] is ok (length of 'bmi104035
      is 9. However if your outcome was named 'adiposity' this would give 
      a string length of 'adiposity104035 = 15' which is too long. I realise
      this is quite annoying. To get round it rename your outcome variable
      to have a shorter name. As a rule of thumb I would rename your outcome to be
      no more than three characters",
      call. = FALSE
    )
  }

  message("DONE", appendLF = TRUE)
  ## ---- ds.Boole ---------------------------------------------------------------

  message("** Step 2 of 7: Defining subsets ... ", appendLF = FALSE)

  # Use each row from this table in a call to ds.Boole. Here we make vectors
  # indicating whether or not the value meets the evaluation criteria

  cats %>%
    pmap(function(value, op, new_df_name, ...) {
      ds.Boole(
        V1 = paste0(new_df, "$", "age"),
        V2 = value,
        Boolean.operator = op,
        newobj = new_df_name,
        datasources = conns[valid_coh]
      )
    })

  ## ---- Create second table with assign conditions -----------------------------
  suppressMessages(
    assign_conditions <- cats %>%
      group_by(varname) %>%
      summarise(condition = paste(new_df_name, collapse = "*"))
  )

  ## ---- Assign variables indicating membership of age band ---------------------
  assign_conditions %>%
    pmap(function(condition, varname) {
      ds.assign(
        toAssign = condition,
        newobj = varname,
        datasources = conns[valid_coh]
      )
    })

  ## ---- Now we want to find out which cohorts have data ------------------------
  data_sum <- assign_conditions %>%
    pmap(function(varname, ...) {
      ds.mean(varname, datasources = conns[valid_coh])
    })

  ## ---- Handle disclosure issues -----------------------------------------------

  # Need to only show data as being available if >= minimum value for subsetting
  sub_min <- ds.listDisclosureSettings(datasources = conns[valid_coh])$ds.disclosure.settings %>%
    map_df(~ .$nfilter.subset)

  min_perc_vec <- sub_min / data_sum[[1]]$Mean.by.Study[, "Ntotal"]

  min_perc <- min_perc_vec %>%
    map_df(~ rep(., times = length(subnames)))

  if (length(valid_coh) == 1) {
    data_available <- data_sum %>%
      map(function(x){x$Mean.by.Study[, "EstimatedMean"]}) %>%
      unlist() %>%
      as_tibble() %>%
      rename(!!valid_coh := value)
    
  } else if (length(valid_coh) > 1) {
    data_available <- data_sum %>%
      map_dfr(function(x){x$Mean.by.Study[, "EstimatedMean"]})
    
  }

  data_available <- as_tibble(ifelse(data_available <= min_perc, "no", "yes")) %>%
    mutate(varname = assign_conditions$varname) %>%
    select(varname, everything())

  ## ---- Create a new table listing which subsets to create ---------------------
  cats_to_subset <- data_available %>%
    pivot_longer(
      cols = -varname,
      names_to = "cohort",
      values_to = "available"
    ) %>%
    filter(available == "yes") %>%
    select(-available) %>%
    mutate(new_subset_name = paste0(varname, "_a"))

  if (nrow(cats_to_subset) < 1) {
    stop("There is no data available within the specified bands",
      call. = FALSE
    )
  }

  message("DONE", appendLF = TRUE)

  ## ---- Create subsets ---------------------------------------------------------
  message("** Step 3 of 7: Creating subsets ... ", appendLF = FALSE)

  cats_to_subset %>%
    pmap(
      function(varname, cohort, new_subset_name, ...) {
        ds.dataFrameSubset(
          df.name = new_df,
          V1.name = varname,
          V2.name = "1",
          Boolean.operator = "==",
          keep.NAs = TRUE,
          newobj = new_subset_name,
          datasources = conns[cohort]
        )
      }
    )

  message("DONE", appendLF = TRUE)

  ## ---- Sort subsets -----------------------------------------------------------
  message("** Step 4 of 7: Dealing with subjects with multiple observations within age bands ... ",
    appendLF = FALSE
  )

  if (mult_action == "nearest") {

    ## Make a variable specifying distance between age of measurement and prefered
    ## value (provided by "mult_vals")

    johan_sort <- tibble(
      subset = unique(cats$varname),
      ref_val = mult_vals
    )

    cats_to_subset %<>%
      mutate(
        ref_val = johan_sort$ref_val[
          match(
            as.character(bmi_to_subset$varname),
            as.character(johan_sort$subset)
          )
        ],
        condition = paste0(
          "((", new_subset_name, "$", "age", "-", ref_val, ")", "^2",
          ")", "^0.5"
        ),
        dif_val = paste0("d_", ref_val)
      )

    cats_to_subset %>%
      pmap(function(condition, cohort, dif_val, ...) {
        ds.make(
          toAssign = condition,
          newobj = dif_val,
          datasources = conns[cohort]
        )
      })

    ## Join this variable back with the dataset
    cats_to_subset %>%
      pmap(function(dif_val, new_subset_name, varname, cohort, ...) {
        ds.dataFrame(
          x = c(new_subset_name, dif_val),
          newobj = paste0(varname, "_y"),
          datasources = conns[cohort]
        )
      })

    ## Sort by it
    cats_to_subset %>%
      pmap(function(cohort, new_subset_name, varname, dif_val, ...) {
        ds.dataFrameSort(
          datasources = conns[cohort],
          df.name = paste0(varname, "_y"),
          sort.key.name = paste0(varname, "_y", "$", dif_val),
          newobj = paste0(varname, "_a"),
          sort.descending = FALSE
        )
      })
  } else if (mult_action == "earliest" | mult_action == "latest") {
    sort_action <- ifelse(mult_action == "earliest", FALSE, TRUE)

    cats_to_subset %>%
      pmap(function(cohort, new_subset_name, varname, ...) {
        ds.dataFrameSort(
          df.name = new_subset_name,
          sort.key.name = paste0(new_subset_name, "$age"),
          newobj = paste0(varname, "_a"),
          sort.descending = sort_action,
          datasources = conns[cohort]
        )
      })
  }

  message("DONE", appendLF = TRUE)

  message("** Step 5 of 7: Reshaping to wide format ... ", appendLF = FALSE)
  ## Now we create variables indicating the age of subset
  cats_to_subset %<>%
    mutate(
      value = str_extract(varname, "[^_]+$"),
      age_cat_name = paste0(varname, "_age")
    )

  cats_to_subset %>%
    pmap(
      function(cohort, new_subset_name, value, age_cat_name, varname, ...) {
        ds.assign(
          toAssign = paste0("(", paste0(varname, "_a"), "$age*0)+", value),
          newobj = age_cat_name,
          datasources = conns[cohort]
        )
      }
    )

  ## ---- Join age variables with subsets ----------------------------------------
  cats_to_subset %>%
    pmap(function(varname, cohort, age_cat_name, ...) {
      ds.dataFrame(
        x = c(paste0(varname, "_a"), age_cat_name),
        newobj = paste0(varname, "_c"),
        datasources = conns[cohort]
      )
    })

  ## ---- Convert subsets to wide form -------------------------------------------
  cats_to_subset %>%
    pmap(
      function(cohort, varname, age_cat_name, ...) {
        ds.reShape(
          data.name = paste0(varname, "_c"),
          timevar.name = age_cat_name,
          idvar.name = "child_id",
          v.names = c(outcome, "age"),
          direction = "wide",
          newobj = paste0(varname, "_wide"),
          datasources = conns[cohort]
        )
      }
    )


  ## ---- Remove NA variables from dataframes ------------------------------------

  ## First we identify the variables we want to keep
  all_vars <- cats_to_subset %>%
    pmap(function(varname, cohort, ...) {
      ds.colnames(paste0(varname, "_wide"), datasources = conns[cohort])[[1]]
    })

  names(all_vars) <- cats_to_subset$cohort

  keep_vars <- all_vars %>%
    map(~ .[str_detect(., ".NA") == FALSE])

  var_list <- split(cats_to_subset$varname, seq(nrow(cats_to_subset)))
  coh_list <- split(cats_to_subset$cohort, seq(nrow(cats_to_subset)))

  combined <- list(var_list, coh_list, keep_vars)
  names(combined) <- c("varname", "cohort", "keep_vars")

  combined %>%
    pmap(function(varname, cohort, keep_vars) {
      dh.dropCols(
        conns = conns[cohort],
        df = paste0(varname, "_wide"),
        vars = keep_vars,
        new_df_name = paste0(varname, "_wide"),
        comp_var = "child_id",
        type = "keep"
      )
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
          by.x.names = "child_id",
          by.y.names = "child_id",
          all.x = TRUE,
          newobj = out_name,
          datasources = conns[.y]
        )
      }

      if (length(.x) > 2) {
        ds.merge(
          x.name = .x[[1]],
          y.name = .x[[2]],
          by.x.names = "child_id",
          by.y.names = "child_id",
          all.x = TRUE,
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
              by.x.names = "child_id",
              by.y.names = "child_id",
              all.x = TRUE,
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
      by.x.names = "child_id",
      by.y.names = "child_id",
      all.x = TRUE,
      newobj = out_name,
      datasources = conns[valid_coh]
    )
  }

  message("DONE", appendLF = TRUE)


  ## ---- Tidy environment -------------------------------------------------------
  message("** Step 7 of 7: Removing temporary objects ... ", appendLF = FALSE)

  end_objs <- ds.ls(datasources = conns)

  to_remove <- unique(end_objs[[1]][!end_objs[[1]] %in% start_objs[[1]]])

  ## but we keep the final dataset
  to_remove <- to_remove[!(to_remove %in% out_name)]

  dh.tidyEnv(conns = conns, obj = to_remove, type = "remove")

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

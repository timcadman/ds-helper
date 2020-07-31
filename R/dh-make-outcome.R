#' Derives one or more outcome variable(s) from repeated measures data
#' 
#' Many analyses will want to use outcomes derived at a single time point,
#' e.g. BMI between ages 10-14. This function automates the process to do this
#' which is quite complex in DataSHIELD. Note that for big datasets this takes
#' a long time to run.
#' 
#' @param df opal dataframe
#' @param outcome name of repeated measures outcome variable
#' @param age_var Vector of values indicating pairs of low and high values
#'             for which to derive outcome variables for. Must be even length
#' @param bands vector of alternating lower and upper age bands for variable(s)
#'              you want to create           
#' @param mult_action if a subject has more than one value within the time 
#'                    period do we keep the earliest or latest? Default = 
#'                    "earliest"
#' @param mult_vals if "mult_action = nearest", this argument specifies which 
#'                  which value in each age band to chose values closest to
#'                  in case of multiple values
#'                  
#' @return a dataset containing the newly derived variables
#' 
#' @importFrom dsBaseClient ds.colnames
#' @importFrom purrr pmap
#' 
#' @author Tim Cadman
#' 
#' @export                                      
dh.makeOutcome <- function(
  df, outcome, age_var, bands, mult_action = c("earliest", "latest", "nearest"),
  mult_vals = NULL){
  
  mult_action <- match.arg(mult_action)
  
## ---- Store current object names ---------------------------------------------
  start_objs <- ds.ls()  
  
## ---- Argument checks --------------------------------------------------------  
  dh.doVarsExist(df, outcome)
  dh.doesDfExist(df)
  
## ---- Create numeric version of age_var --------------------------------------  
  ds.asNumeric(
    x.name = paste0(df, "$", age_var), 
    newobj = "age_n"
  )
  
  names(opals) %>%
    map(
      ~ds.dataFrame(
        x= c(df, "age_n"), 
        newobj = df, 
        datasources = opals[.])
    )
  
## ---- Make paired list for each band -----------------------------------------  
  pairs <- split(bands, ceiling(seq_along(bands)/2))
  
  subnames <- unlist(
    pairs %>% map(~paste0(outcome, "_", paste0(., collapse = "_"))),
    use.names = FALSE
  )
  
  ## ---- Create table with age bands ------------------------------------------
  cats <- tibble(
    varname = rep(subnames, each = 2),
    value = ages,
    op = rep(c(">=", "<"), times = (length(ages)/2)),
    new_df_name = paste0(outcome, value)
  )
  
  
  ## ---- Check max character length -------------------------------------------
  if(max(nchar(cats$varname)) + 6 > 20){
    
    stop(
"Due to disclosure settings, the total string length of [outcome] + 
[max(lower_band)] + [max(upper_band)] + [max(mult_vals)] must no more 
than 14 characters. For example: [outcome = 'bmi', max(low_band) = 10, 
max(upper_band) = 40, max(mult_vals) = 35] is ok (length of 'bmi104035
is 9. However if your outcome was named 'adiposity' this would give 
a string length of 'adiposity104035 = 15' which is too long. I realise
this is quite annoying. To get round it rename your outcome variable
to have a shorter name.")
  }
  
  cat("This may take some time depending on the number and size of datasets\n\n")
  
  
  ## ---- ds.Boole ---------------------------------------------------------------

# Use each row from this table in a call to ds.Boole. Here we make ten vectors
# indicating whether or not the value meets the evaluation criteria

message("** Step 1 of 6: Defining subsets ... ", appendLF = FALSE)

  cats %>%
  pmap(function(value, op, new_df_name, ...){
    ds.Boole(
      V1 = paste0(df, "$", "age_n"), 
      V2 = value, 
      Boolean.operator = op, 
      newobj = new_df_name)
  })


## ---- Create second table with assign conditions -----------------------------
suppressMessages(  
  assign_conditions <- cats %>%
  group_by(varname) %>%
  summarise(condition = paste(new_df_name, collapse="*")) 
)

## ---- Assign variables indicating membership of age band ---------------------
assign_conditions %>%
  pmap(function(condition, varname){
    ds.assign(
      toAssign = condition, 
      newobj = varname)
  })

  message("DONE", appendLF = TRUE)

## ---- Now we want to find out which cohorts have data ------------------------
  message("** Step 2 of 6: Creating subsets ... ", appendLF = FALSE)
  
data_available <- assign_conditions %>%
  pmap(function(varname, ...){ds.mean(varname)}) 

data_available <- map_dfr(
  data_available, ~.x$Mean.by.Study[, "EstimatedMean"]) %>%
  map_dfr(~ifelse(.x == 0, "no", "yes")) %>%
  mutate(varname = assign_conditions$varname) %>%
  select(varname, everything())

## ---- Create a new table listing which subsets to create ---------------------
cats_to_subset <- data_available %>%
  pivot_longer(
    cols = -varname, 
    names_to = "cohort", 
    values_to = "available") %>%
  filter(available == "yes") %>%
  select(-available) %>%
  mutate(new_subset_name = paste0(varname, "_a"))

## ---- Create subsets ---------------------------------------------------------
cats_to_subset %>%
  pmap(
    function(varname, cohort, new_subset_name){
      ds.dataFrameSubset(
        df.name = "monthrep", 
        V1.name = varname, 
        V2.name = "1", 
        Boolean.operator = "==", 
        keep.NAs = FALSE, 
        newobj = new_subset_name, 
        datasources = opals[cohort])
    })

message("DONE", appendLF = TRUE)

## ---- Sort subsets -----------------------------------------------------------
message("** Step 3 of 6: Dealing with subjects with multiple observations within age bands ... ", 
        appendLF = FALSE)

if(mult_action == "nearest"){

  ## Make a variable specifying distance between age of measurement and prefered
  ## value (provided by "mult_vals")
  
  johan_sort <- tibble(
    subset = unique(cats$varname),
    ref_val = mult_vals
  )
  
  cats_to_subset %<>%
    mutate(ref_val = johan_sort$ref_val[
      match(
        as.character(bmi_to_subset$varname), 
        as.character(johan_sort$subset))], 
      condition = paste0("((", new_subset_name, "$", "age_n", "-", ref_val, ")", "^2", 
                         ")", "^0.5"), 
          dif_val = paste0("d_", ref_val)
      )
    
  cats_to_subset %>%
    pmap(function(condition, cohort, dif_val, ...){
      ds.make(
        toAssign = condition, 
        newobj = dif_val, 
        datasources = opals[cohort])
    })
  
  ## Join this variable back with the dataset
  cats_to_subset %>%
    pmap(function(dif_val, new_subset_name, varname, cohort, ...){
      ds.dataFrame(
        x= c(new_subset_name, dif_val), 
        newobj = paste0(varname, "_y"), 
        datasources = opals[cohort])
    })
  
  ## Sort by it
  cats_to_subset %>%
    pmap(function(cohort, new_subset_name, varname, dif_val, ...){
      
      ds.dataFrameSort(
        df.name = paste0(varname, "_y"), 
        sort.key.name = paste0(varname, "_y", "$", dif_val), 
        newobj = paste0(varname, "_b"), 
        sort.descending = FALSE)
      
    })

} else if(mult_action == "earliest" | mult_action == "latest"){

sort_action <- ifelse(mult_action == "earliest", FALSE, TRUE)

cats_to_subset %>%
  pmap(function(cohort, new_subset_name, varname, ...){
    
    ds.dataFrameSort(
      df.name = new_subset_name, 
      sort.key.name = paste0(new_subset_name, "$age_n"), 
      newobj = paste0(varname, "_b"), 
      sort.descending = sort_action, 
      datasources = opals[cohort])
    
  })

}

message("DONE", appendLF = TRUE)

message("** Step 4 of 6: Reshaping to wide format ... ", appendLF = FALSE)
## Now we create variables indicating the age of subset
cats_to_subset %<>%
  mutate(value = str_extract(varname, '[^_]+$'), 
         age_cat_name = paste0(varname, "_age"))

cats_to_subset %>%
  pmap(
    function(cohort, new_subset_name, value, age_cat_name, varname, ...){
      ds.assign(
        toAssign = paste0("(", paste0(varname, "_b"), "$age_months * 0)+", value), 
        newobj = age_cat_name, 
        datasources = opals[cohort])
    })


## ---- Join age variables with subsets ----------------------------------------
cats_to_subset %>%
  pmap(function(varname, cohort, age_cat_name, ...){
    
    ds.dataFrame(
      x = c(paste0(varname, "_b"), age_cat_name), 
      newobj = paste0(varname, "_c"),
      datasources = opals[cohort]
    )}
  )

## ---- Convert subsets to wide form -------------------------------------------
cats_to_subset %>%
  pmap(
    function(cohort, varname, age_cat_name, ...){
      ds.reShape(
        data.name = paste0(varname, "_c"),
        timevar.name = age_cat_name,
        idvar.name = "child_id",
        v.names = c(outcome, "age_months"), 
        direction = "wide", 
        newobj = paste0(varname, "_wide"),
        datasources = opals[cohort])
    })

message("DONE", appendLF = TRUE)

## ---- Merge back with non-repeated dataset -----------------------------------
message("** Step 5 of 6: Creating final dataset ... ", appendLF = FALSE)

suppressMessages(  
made_vars <- bmi_to_subset %>%
  arrange(cohort) %>%
  group_by(cohort) %>%
  summarise(subs = paste(varname, collapse = ",")) %>%
  select(subs) %>%
  map(~strsplit(., ","))
)

finalvars <- made_vars$sub %>% map(~paste0(., "_wide"))

names(finalvars) <- sort(names(opals))

out_name <- paste0(outcome, "_", "derived")

finalvars %>%
  imap(function(.x, .y){
    
    if(length(.x) == 1){
      
      ds.dataFrame(
        x = .x, 
        newobj = out_name,
        datasources = opals[.y])
      
    }
    
    if(length(.x) == 2){
      
      ds.merge(
        x.name = .x[[1]],
        y.name = .x[[2]],
        by.x.names = "child_id",
        by.y.names = "child_id",
        all.x = TRUE,
        newobj = out_name, 
        datasources = opals[.y])
      
    }
    
    if(length(.x) >2){
      
      ds.merge(
        x.name = .x[[1]],
        y.name = .x[[2]],
        by.x.names = "child_id",
        by.y.names = "child_id",
        all.x = TRUE,
        newobj = out_name, 
        datasources = opals[.y])
      
      remaining <- tibble(
        dfs = .x[3: length(.x)], 
        cohort = rep(.y, length(dfs)))
      
      remaining %>%
        pmap(function(dfs, cohort){
          ds.merge(
            x.name = out_name,
            y.name = dfs,
            by.x.names = "child_id",
            by.y.names = "child_id",
            all.x = TRUE,
            newobj = out_name, 
            datasources = opals[cohort])
          
        })
      
    }
    
  })

message("DONE", appendLF = TRUE)

## ---- Tidy environment -------------------------------------------------------
message("** Step 6 of 6: Removing temporary objects ... ", appendLF = FALSE)

end_objs <- ds.ls()  

to_remove <- unique(end_objs[[1]][! end_objs[[1]] %in% start_objs[[1]]])
  
## but we keep the final dataset
to_remove <- to_remove[!(to_remove %in% out_name)]

dh.tidyEnv(obj = to_remove, type = "remove")

message("DONE", appendLF = TRUE)

cat("\nDataframe", "'", out_name, "'", 
    "created containing the following variables:\n\n")
data_available

}

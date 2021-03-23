#' Derives one or more outcome variable(s) from repeated measures data
#' 
#' When running multilevel models we often want to visualise the actual data,
#' for example the average outcome at different time points. This is useful to
#' check visually how well our model fits the data. This function does that. At 
#' the moment this is a really slow, convoluted way to do it involving taking 
#' lots of subsets. This is because tapply.assign doesn't work properly at the
#' moment. 
#' 
#'
#' @param df serverside dataframe
#' @param age_var variable indicating subject age/time
#' @param outcome outcome variable 
#' @param bands vector of even length given the upper and low age bands to 
#'              create the outcomes at. 
#' @param grouping an optional variable if you want to stratify the means by 
#'        another variable, e.g. socioeconomic position.              
#' @param conns connections object for DataSHIELD backends
#'
#' @return a tibble containing mean values of the outcome at different age bands
#'
#' @importFrom dsBaseClient ds.colnames ds.asFactor ds.levels ds.dataFrameSubset
#' @importFrom purrr map imap map_df compact flatten
#' @importFrom stringr str_detect
#' @importFrom dplyr bind_rows select filter mutate left_join
#' @importFrom tidyr pivot_wider pivot_longer separate
#' 
#' @export
dh.meanByOccasion <- function(df, age_var, outcome, bands, conns = NULL, grouping = NULL) {

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  
  suff_form <- cohort <- variable <- age <- occage <- value <- . <- 
    factorlevel <- NULL
  ##############################################################################
  # 1. Preamble
  ##############################################################################

  ## Create a list of suffixes so we can identify the variables we've created
  # suffixes <- bands %>% map(~.[c(FALSE, TRUE)]) %>% unlist
  # suff_form <- paste(paste0(".", suffixes), collapse = "|")

  ##############################################################################
  # 2. First option if we don't want to stratify by another variable
  ##############################################################################

  ## Here we just create subsets based on our bands
  if (is.null(grouping)) {
    bands %>% imap(
      ~ dh.makeOutcome(
        df = df,
        outcome = outcome,
        age_var = age_var,
        bands = .x,
        df_name = "tmp_df",
        keep_original = TRUE,
        conns = conns[.y]
      )
    )

    ## Vector of all variables we've created
    varsmade <- ds.colnames("tmp_df", datasources = conns)

    ## Subset of ones we want to keep based on naming convention that the new
    ## variable names will be based on the upper band provided
    tokeep <- varsmade %>% map(function(x) {
      tmpkeep <- str_detect(string = x, pattern = suff_form)
      out <- x[tmpkeep]
      return(out)
    })

    ## Now use getStats to summarise these
    preout <- tokeep %>%
      imap(
        ~ dh.getStats(
          df = "tmp_df",
          vars = .x,
          conns = conns[.y]
        )
      )

    out <- preout %>%
      map(compact) %>%
      flatten() %>%
      bind_rows() %>%
      select(cohort, variable, mean) %>%
      separate(variable, sep = "['.'']", into = c("variable", "age")) %>%
      filter(cohort != "Combined") %>%
      mutate(occage = age) %>%
      select(-age) %>%
      pivot_wider(
        names_from = variable,
        values_from = mean,
        id_cols = c(cohort, occage)
      )

    ##############################################################################
    # 3. More complicated option if wanting to stratify by another variable
    ##############################################################################
  } else if (!is.null(grouping)) {
    ## First we turn the grouping variable into a factor (it may already be)
    names(conns) %>%
      map(
        ~ ds.asFactor(
          paste0(df, "$", grouping),
          newobj.name = "tmpfac",
          datasources = conns[.]
        )
      )

    levels <- ds.levels("tmpfac", datasources = conns) %>%
      map_df(function(x) {
        x$Levels
      }) %>%
      pivot_longer(cols = everything())

    ## We use this tibble of levels to create subsets stratified by our grouping
    ## variable
    levels %>%
      pmap(function(name, value) {
        ds.dataFrameSubset(
          df.name = df,
          V1.name = paste0(df, "$", grouping),
          V2.name = value,
          Boolean.operator = "==",
          newobj = paste0("tmp", "_", value),
          datasources = conns[name]
        )
      })


    ## Make vector of subset names
    presub <- levels %>%
      mutate(
        strat_dfs = paste0("tmp_", value)
      )

    bands_tib <- bands %>%
      map(., paste, collapse = ",") %>%
      bind_rows() %>%
      pivot_longer(cols = everything()) %>%
      mutate(bands = value) %>%
      select(-value)

    tmpytmp <- left_join(presub, bands_tib, by = "name")

    tmpytmp %>%
      pmap(function(name, value, strat_dfs, bands) {
        dsHelper::dh.makeOutcome(
          df = strat_dfs,
          outcome = outcome,
          age_var = age_var,
          bands = unlist(strsplit(bands, split = ",")),
          df_name = paste0("zzz_", value),
          keep_original = TRUE,
          mult_action = "earliest",
          conns = conns[name]
        )
      })

    ## Create a list of all the variables in the subsets
    varsmade <- tmpytmp %>%
      pmap(function(name, value, ...) {
        ds.colnames(
          x = paste0("zzz_", value),
          datasources = conns[name]
        )
      })

    names(varsmade) <- paste0("zzz_", tmpytmp$value)

    ## Use this list to subset the list of variables
    tokeep <- varsmade %>%
      map(function(x) {
        map(x, function(y) {
          y[str_detect(y, pattern = "(\\.[\\d]+)")]
        })
      })

    ## Two many list levels to keep in my fragile mind so putting into a tibble.
    ## Less efficient but easier to implement
    tokeep_tib <- tokeep %>%
      bind_rows(.id = "df") %>%
      pivot_longer(cols = c(names(conns))) %>%
      mutate(level = str_extract(df, "([^_]+$)")) %>%
      filter(!is.na(value))


    ## Not a neat way of doing it, but now we filter out anything that isn't the
    ## outcome or age variables
    tokeep_tib_filt <- tokeep_tib %>%
      filter(str_detect(value, pattern = paste0(outcome, "|age")) == TRUE)


    ## Ok almost here - just need to tweak above so we just have a row for
    ## each cohort - have to change because not all cohorts have same dfs now
    fortab <- tokeep_tib_filt %>%
      pmap(function(df, name, value, ...) {
        dsHelper::dh.getStats(
          df = df,
          vars = value,
          conns = conns[name]
        )
      })


    ## Now just sort this out so we can keep track of the levels of the grouping
    ## variable
    tmp <- fortab %>%
      map(compact) %>%
      flatten()
    names(tmp) <- tokeep_tib_filt$level
    preout <- tmp %>% bind_rows(.id = "factorlevel")

    out <- preout %>%
      separate(variable, sep = "['.'']", into = c("variable", "age")) %>%
      filter(cohort != "Combined") %>%
      mutate(occage = age) %>%
      select(-age) %>%
      pivot_wider(
        names_from = variable,
        values_from = mean,
        id_cols = c(cohort, occage, factorlevel)
      )
  }

  return(out)
}

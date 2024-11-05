#' Given df-A & df-B, creates a new df which is the rows in A but not in B
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' Often we need to exclude participants in an analysis. This function created a 
#' dataframe containing participants in \code{original_df} but not in \code{final_df}. It was 
#' deprecated because you can now use \code{dsTidyverseClient::ds.filter}, which is much quicker
#' and more flexible.
#'
#' @param original_df Dataframe containing the full sample
#' @param final_df Dataframe containing the included sample
#' @param type Specifies type of dataframe in `original_df`. Either 'long' or
#' 'wide'. NOTE NOT CURRENTLY FUNCTIONAL - ONLY WORKS FOR WIDE.
#' @template id_var
#' @template new_obj
#' @template conns
#' @return Creates a serverside dataframe containing the rows from `original_df`
#' that are not contained in `final_df`
#' @importFrom utils head
#' @keywords internal
#' @export
dh.makeExcludedDf <- function(original_df, final_df, id_var = "child_id", new_obj,
                              type = "wide", conns = NULL) {
  lifecycle::deprecate_warn("1.6.0", "dh.makeExcludedDf()", "dsTidyverseClient::ds.filter()")
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dims <- ds.dim(final_df)

  length.ref <- tibble(
    cohort = names(conns),
    length = dims %>% map(~ .x[[1]]) %>% unlist() %>% head(-1) %>% as.character()
  )

  length.ref %>%
    pmap(function(cohort, length) {
      ds.rep(
        x1 = 1,
        times = length,
        source.x1 = "clientside",
        source.times = "c",
        newobj = "final_ones",
        datasources = conns[cohort]
      )
    })

  ds.dataFrame(
    x = c(paste0(final_df, "$", id_var), "final_ones"),
    newobj = "final_tmp"
  )

  ## ---- Now merge this vector with baseline_df ---------------------------------

  if (type == "wide") {
    ds.merge(
      x.name = original_df,
      y.name = "final_tmp",
      by.x.names = id_var,
      by.y.names = id_var,
      all.x = TRUE,
      all.y = FALSE,
      newobj = "orig_w_vec"
    )
  } else if (type == "long") {
    ds.merge(
      x.name = original_df,
      y.name = "final_tmp",
      by.x.names = c(id_var, "age"),
      by.y.names = c(id_var, "age"),
      all.x = TRUE,
      all.y = FALSE,
      newobj = "orig_w_vec"
    )
  }

  ## ---- Transform case_def to binary vector ------------------------------------
  ds.Boole(
    V1 = paste0("orig_w_vec$final_ones"),
    V2 = 1,
    Boolean.operator = "==",
    na.assign = "0",
    newobj = "exc_vec"
  )

  ## ---- Create excluded subset based on this vector ----------------------------
  ds.dataFrameSubset(
    df.name = original_df,
    V1.name = "exc_vec",
    V2.name = "0",
    Boolean.operator = "==",
    keep.NAs = FALSE,
    newobj = new_obj
  )
}

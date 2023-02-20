#' Creates z-scores within specified bands
#'
#' Especially with mental health outcomes, we often want to transform raw scores
#' into z-scores, but within certain age bands (e.g. measurement occasion or 
#' per year). This function does this.
#' 
#' @template df 
#' @param out_var Variable to make z-scores for
#' @param age_var Age variable
#' @param low_band Lower band for z score
#' @param upp_band Upper band for z score
#' @template new_obj
#' @template conns
#'
#' @return Z score serverside within specified bands
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom purrr pmap cross2 map_chr
#' @importFrom dsBaseClient ds.cbind
#' @importFrom DSI datashield.connections_find
#'
#' @family trajectory functions
#' @family data manipulation functions
#'
#' @export
dh.zByGroup <- function(df, out_var, age_var, low_band, upp_band, conns, new_obj){
  
  .BooleTwoConditions(
    df = df, 
    var = "age", 
    value_1 = low_band,
    value_2 = upp_band,
    op_1 = ">=", 
    op_2 = "<", 
    newobj = "tmp_1", 
    conns = conns)
  
  ds.recodeValues(
    var.name = "tmp_1", 
    values2replace.vector = 0,
    new.values.vector = NA, 
    newobj = "tmp_2", 
    datasources = conns)
  
  ref <- dh.meanByGroup(
    df = df, 
    outcome = out_var, 
    group_var = age_var, 
    intervals = c(low_band, upp_band), 
    checks = F,
    conns = conns)
  
  assign_form <- paste0(
    "((", 
    paste0(df, "$", out_var), 
    "-", ref$mean, ")", "/", ref$std.dev, ")")
  
  ds.assign(
    toAssign = assign_form,
    newobj = "tmp_3", 
    datasources = conns)
  
  ds.assign(
    toAssign = paste0("tmp_2*tmp_3"),
    newobj = new_obj, 
    datasources = conns)
  
}
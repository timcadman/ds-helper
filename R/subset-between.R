dh.subsetBetween <- function(df = NULL, subset_var = NULL, low_val = NULL, 
                             high_val = NULL, new_df_name = NULL, conns = NULL){
  
  if (is.null(df)) {
    stop("Please specify a data frame")
  }
  
  if (is.null(subset_var)) {
    stop("Please specify a variable to subset on")
  }
  
  if (is.null(low_val)) {
    stop("Please specify the lower value for the subset")
  }
  
  if (is.null(high_val)) {
    stop("Please specify the upper value for the subset")
  }
  
  if (is.null(new_df_name)) {
    stop("Please specify the name of the new subset")
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  ds.Boole(
    V1 = paste0(df, "$", subset_var), 
    V2 = low_val, 
    Boolean.operator = ">=", 
    na.assign = "NA", 
    newobj = "tmp_a", 
    datasources = conns)
  
  ds.Boole(
    V1 = paste0(df, "$", subset_var), 
    V2 = high_val, 
    Boolean.operator = "<", 
    na.assign = "NA", 
    newobj = "tmp_b", 
    datasources = conns)
  
  ds.make(
    toAssign = "tmp_a*tmp_b", 
    newobj = "tmp_c", 
    datasources = conns
  )
  
  ds.dataFrameSubset(
    df.name = "analysis_df", 
    V1.name = "tmp_c", 
    V2.name = "1", 
    Boolean.operator = "==", 
    newobj = new_df_name, 
    datasources = conns
  )
  
  dh.tidyEnv(
    obj = c("tmp_a", "tmp_b", "tmp_c"), 
    type = "remove"
  )
  
}
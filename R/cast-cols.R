#' Casts column classes of a table
#' 
#' This function allows you to specify the desired classes for a some variables
#' of a dataframe. The resulting dataframe will have the same column order as
#' the input one. If multiple columns are to be casted but only one objective class
#' is provided, it will be reused for all the selected columns.
#'
#' @template df
#' @param objective_columns Character vector specifying which columns are to be casted to new classes.
#' @param objective_class Character vector specifying the objective classes of the selected columns.
#' @template conns
#' @template checks
#' 
#' @importFrom dsBaseClient ds.asFactor ds.asCharacter ds.asNumeric ds.asInteger ds.colnames ds.dim ds.rep ds.dataFrameSubset ds.cbind
#' @importFrom DSI datashield.connections_find
#'
#' @return Tibble with a summary of the successful and failed casts
#' @export
dh.columnCast <- function(df = NULL, objective_columns = NULL, objective_class = NULL, conns = NULL, checks = TRUE){
  
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(objective_columns)) {
    stop("`current_names` must not be NULL.", call. = FALSE)
  }
  
  
  if (is.null(objective_class)) {
    stop("`current_names` must not be NULL.", call. = FALSE)
  }
  
  `%notin%` <- Negate(`%in%`)
  if(any(objective_class %notin% c("factor", "character", "numeric", "integer"))){
    stop("", call. = FALSE) # TODO completar missatge correctament
  }
  
  if(length(objective_columns) != length(objective_class) & length(objective_class) == 1){
    objective_class <- rep(objective_class, length(objective_columns))
    warning("") # TODO completar missatge correctament
  } else if (length(objective_columns) != length(objective_class)) {
    stop("", call. = FALSE) # TODO completar missatge correctament
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  if (checks == TRUE) {
    .isDefined(df = df, vars = objective_columns, conns = conns)
  }
  
  # Get column indexes the `objective_columns` have on the `df`
  objective_columns_index <- which(ds.colnames(x = df, datasources = conns)[[1]] %in% objective_columns)
  
  # Perform casting of `objective_columns` to `objective_class`
  casting_results <- lapply(1:length(objective_columns), function(x){
    switch(objective_class[x],
           "factor" = tryCatch({ds.asFactor(input.var.name = paste0(df, "$", objective_columns[x]), 
                                            newobj.name = objective_columns[x], 
                                            datasources = conns);objective_columns[x]}, error = function(w){NULL}),
           "character" = tryCatch({ds.asCharacter(x.name = paste0(df, "$", objective_columns[x]), 
                                                  newobj = objective_columns[x], 
                                                  datasources = conns);objective_columns[x]}, error = function(w){NULL}),
           "numeric" = tryCatch({ds.asNumeric(x.name = paste0(df, "$", objective_columns[x]), 
                                              newobj = objective_columns[x], 
                                              datasources = conns);objective_columns[x]}, error = function(w){NULL}),
           "integer" = tryCatch({ds.asInteger(x.name = paste0(df, "$", objective_columns[x]), 
                                              newobj = objective_columns[x],
                                              datasources = conns);objective_columns[x]}, error = function(w){NULL})
           )
    })
  
  # Get the successful casts
  success_casts <- unlist(lapply(casting_results, function(x){!is.null(x)}))
  
  # Update `objective_columns_index` only with successful casts
  objective_columns_index <- objective_columns_index[success_casts]
  
  # Get the object names for the successful casts
  cast_names <- unlist(casting_results)
  
  # Get subset of original table without the successful casted columns
  times <- ds.dim(df, datasources = conns)[[3]][1]
  ds.rep(x1 = 1,
         times = times,
         source.times = "c",
         source.each = "c",
         newobj = "ONES", 
         datasources = conns)
  ds.dataFrameSubset(df.name = df, 
                     V1.name = "ONES",  
                     V2.name = "ONES",
                     Boolean.operator = "==", 
                     rm.cols = objective_columns_index, 
                     newobj = paste0(df, "_aux_nobj"), 
                     datasources = conns)
  
  # Bind casted columns to the previous table
  ds.cbind(x = c(paste0(df, "_aux_nobj"), cast_names), newobj = paste0(df, "_aux_nobj"), datasources = conns)
  
  # Reorder table to align with original data and overwrite original table
  ds.dataFrameSubset(df.name = paste0(df, "_aux_nobj"), 
                     V1.name = "ONES",  
                     V2.name = "ONES",
                     Boolean.operator = "==", 
                     keep.cols = match(ds.colnames(df, datasources = conns)[[1]], 
                                       ds.colnames(paste0(df, "_aux_nobj"), datasources = conns)[[1]]),
                     newobj = df,
                     datasources = conns)
  
  return(tibble(objective_columns,
         objective_class,
         success = success_casts))
}

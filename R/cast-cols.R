#' Casts column classes of a table
#'
#' This function allows you to specify the desired classes for a some variables
#' of a dataframe. The resulting dataframe will have the same column order as
#' the input one. If multiple columns are to be casted but only one objective class
#' is provided, it will be reused for all the selected columns.
#'
#' @template df
#' @param target_vars Character vector specifying which columns are to be casted to new classes.
#' @param target_class Character vector specifying the objective classes of the selected columns.
#' @template conns
#' @template checks
#' 
#' @importFrom dsBaseClient ds.asFactor ds.asCharacter ds.asNumeric ds.asInteger ds.colnames ds.dim ds.rep ds.dataFrameSubset ds.cbind
#' @importFrom DSI datashield.connections_find
#'
#' @return Tibble with a summary of the successful and failed casts
#' @export
dh.columnCast <- function(df = NULL, target_vars = NULL, target_class = NULL, conns = NULL, checks = TRUE){
  
  if (is.null(df)) {
    stop("`df` must not be NULL.", call. = FALSE)
  }
  
  if (is.null(target_vars)) {
    stop("`current_names` must not be NULL.", call. = FALSE)
  }
  
  
  if (is.null(target_class)) {
    stop("`current_names` must not be NULL.", call. = FALSE)
  }
  
  `%notin%` <- Negate(`%in%`)
  if(any(target_class %notin% c("factor", "character", "numeric", "integer"))){
    stop("", call. = FALSE) # TODO completar missatge correctament
  }
  
  if(length(target_vars) != length(target_class) & length(target_class) == 1){
    target_class <- rep(target_class, length(target_vars))
    warning("") # TODO completar missatge correctament
  } else if (length(target_vars) != length(target_class)) {
    stop("", call. = FALSE) # TODO completar missatge correctament
  }
  
  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }
  
  if (checks == TRUE) {
    .isDefined(df = df, vars = target_vars, conns = conns)
  }
  
  # Get column indexes the `target_vars` have on the `df`
  target_vars_index <- lapply(ds.colnames(x = df, datasources = conns), function(x){
    match(target_vars, x)
  })
  
  # Perform casting of `target_vars` to `target_class`
  casting_results <- lapply(1:length(target_vars), function(x){
    switch(target_class[x],
           "factor" = tryCatch({ds.asFactor(input.var.name = paste0(df, "$", target_vars[x]), 
                                            newobj.name = target_vars[x], 
                                            datasources = conns);target_vars[x]}, error = function(w){NULL}),
           "character" = tryCatch({ds.asCharacter(x.name = paste0(df, "$", target_vars[x]), 
                                                  newobj = target_vars[x], 
                                                  datasources = conns);target_vars[x]}, error = function(w){NULL}),
           "numeric" = tryCatch({ds.asNumeric(x.name = paste0(df, "$", target_vars[x]), 
                                              newobj = target_vars[x], 
                                              datasources = conns);target_vars[x]}, error = function(w){NULL}),
           "integer" = tryCatch({ds.asInteger(x.name = paste0(df, "$", target_vars[x]), 
                                              newobj = target_vars[x],
                                              datasources = conns);target_vars[x]}, error = function(w){NULL})
           )
    })
  
  # Get the successful and failed casts
  success_casts <- unlist(lapply(casting_results, function(x){!is.null(x)}))
  failed_casts <- !success_casts
  warning(paste0("[", paste(target_vars[failed_casts], collapse = ", "), "] column(s) have failed, they will keep their previous class"),
          call. = FALSE)
  
  # Update `target_vars_index` only with successful casts
  target_vars_index <- lapply(target_vars_index, function(x){
    x[success_casts]
  })
  
  # Get the object names for the successful casts
  cast_names <- unlist(casting_results)
  
  # Get subset of original table without the successful casted columns
  times <- tail(ds.dim(df, datasources = conns), 1)[1]
  ds.rep(x1 = 1,
         times = times,
         source.times = "c",
         source.each = "c",
         newobj = "ONES", 
         datasources = conns)
  lapply(1:length(target_vars_index), function(x){
    ds.dataFrameSubset(df.name = df, 
                       V1.name = "ONES",  
                       V2.name = "ONES",
                       Boolean.operator = "==", 
                       rm.cols = target_vars_index[[x]], 
                       newobj = paste0(df, "_aux_nobj"), 
                       datasources = conns[x])
  })
  
  # Bind casted columns to the previous table
  ds.cbind(x = c(paste0(df, "_aux_nobj"), cast_names), newobj = paste0(df, "_aux_nobj"), datasources = conns)
  
  # Reorder table to align with original data and overwrite original table
  lapply(1:length(target_vars_index), function(x){
    ds.dataFrameSubset(df.name = paste0(df, "_aux_nobj"), 
                       V1.name = "ONES",  
                       V2.name = "ONES",
                       Boolean.operator = "==", 
                       keep.cols = match(ds.colnames(df, datasources = conns[x])[[1]], 
                                         ds.colnames(paste0(df, "_aux_nobj"), datasources = conns[x])[[1]]),
                       newobj = df,
                       datasources = conns[x])
  })
  
  return(tibble(target_vars,
         target_class,
         success = success_casts))
}

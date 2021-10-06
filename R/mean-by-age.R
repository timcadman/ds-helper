#' Calculate mean values of one repeated measures variable by an age variable

#' @param conns connection object for DataSHIELD backends
#' @param df datashield dataframe
#' @param outcome outcome variable in long format
#' @param age_var age in years
#'
#' @return Mean values for each unit of your age variable are returned
#'
#' @importFrom ds


dh.meanByAge <- function(df = NULL, outcome = NULL, age_var = NULL, conns = NULL){

  if (is.null(df)) {
    stop("Please specify a data frame")
  }

  if (is.null(outcome)) {
    stop("Please specify an outcome variable")
  }

  if (is.null(age_var)) {
    stop("Please specify a grouping age/time variable")
  }

  if (is.null(conns)) {
    conns <- datashield.connections_find()
  }

  dh.doesDfExist(conns = conns, df = df)
  dh.doVarsExist(conns = conns, vars = c(outcome, age_var), df = df)


## ---- First we round up the age variable -----------------------------------------------
DSI::datashield.assign(conns, "age_tmp", as.symbol(paste0(df, "$", age_var, "+0.5")))

calltext <- call("asIntegerDS", "age_tmp")
DSI::datashield.assign(conns, "age_round", calltext)

## ---- Now we get the mean values for the outcome by age --------------------------------
calltext <- paste0("meanSdGpDS(", paste0(df, "$", outcome), ",", "age_round", ")")
mean_tmp <- DSI::datashield.aggregate(conns, as.symbol(calltext))


mean_tmp[[1]]$Mean_gp


## ---- Now put into neat long format -------------------------------------------------------
out <- mean_tmp %>%
map(function(x){

x$Mean_gp %>% as_tibble(rownames = "age") %>%
mutate(age = as.numeric(str_remove(age, "age_round_"))) 
}) %>%
bind_rows(.id = "cohort")


## ---- Remove temporary objects ------------------------------------------------------------
dh.tidyEnv(
	obj = c("age_tmp", "age_round"),
  type = "remove"
)

return(out)

}
#' Produces descriptive statistics based on repeated measures data
#' which it would be useful to report in papers.
#'
#' @param df datashield dataframe
#' @param age_var name of age variable in df
#' @param outcome name of outcome variable in df
#' @param conns connection object for DataSHIELD backends
#' 
#' @importFrom dplyr %>%
#'
#' @export

dh.getRmStats <- function(df = NULL, outcome = NULL, age_var = NULL, conns = NULL){

if(is.null(df)){
stop("Please provide the name of a datashield dataframe")
}

if(is.null(outcome)){
stop("Please provide the name of your outcome variable")
}

if(is.null(age_var)){
stop("Please provide the name of your age variable in df")
}

if (is.null(conns)) {
conns <- datashield.connections_find()
}


df = "data"
outcome = "weight"
age_var = "age"

## ---- First get overall stats for some of the easy ones -------------------------------------------
stats <- dh.getStats(
 	df = df,
 	vars = c(outcome, age_var)
 )

## ---- Age range of participants -------------------------------------------------------------------
 age_ranges <- stats$continuous %>%
 dplyr::filter(variable == age_var) %>%
 mutate(
 	min_age = perc_5,
 	max_age = perc_95) %>%
 dplyr::select(cohort, min_age, max_age)

## ---- Total number of outcome measurements -------------------------------------
outcome_n <- stats$continuous %>%
dplyr::filter(variable == outcome) %>%
dplyr::select(cohort, n_obs = valid_n)


## ---- Total number of unique participants ----------------------------------------

# First, we use ds.tapply.assign to summarise the number of observations for each
# subject. The length of this created object then gives us the number of subjects.

ds.summary("data$id_int")

ds.asFactorSimple("data$id_int", "id_fact")

ds.summary("id_fact")

ds.tapply.assign(
	X.name = "data$weight",
 	INDEX.names = "id_fact",
 	FUN.name = "N",
 	newobj = "id_summary")

ds.length("id_summary")
ds.class("id_summary")

ds.tapply(
	X.name = 'data$weight',
 	INDEX.names = 'data$id_int',
 	FUN.name = 'N')

n_subjects <- ds.length("id_summary$N")[1:length(names(conns))] %>%
 setNames(names(conns)) %>%
 bind_rows() %>%
 mutate(
 	variable = "No. participants",
 	category = "") %>%
 select(variable, everything()) %>%
 mutate(across(where(is.numeric), as.character))


### Median number of weight measurements per child
# We can use the ds.quantileMean function with the object we created above (number
# of measurements by id) to get the median number of measurements per child.
# ```{r summarise-median-measurements}
# ds.asNumeric("id_summary$N", "id_summary_num")

# weight_med_iqr <- ds.quantileMean("id_summary_num", type = "split") %>%
# bind_rows(.id = "cohort") %>%
# select(cohort, "5%", "50%", "95%") %>%
# rename(median = "50%", perc_5 = "5%", perc_95 = "95%") %>%
# mutate(
# 	iqr = perc_95 - perc_5,
# 	med_iqr = paste0(median, " (", iqr, ")"),
# 	variable = "Median no. measures per child (IQR)",
# 	category = "") %>%
# select(variable, category, cohort, med_iqr) %>%
# pivot_wider(names_from = cohort, values_from = med_iqr)


### Neaten up our extracted stats
# We take the objects we created earlier with statistics on our exposures and
# covariates, select the summary information we want, reshape into wide format
# and relabel our categories.
# ```{r summarise-categorical}
# cat_sum <- cat_stats %>%
# select(variable, cohort, category, perc_total) %>%
# filter(cohort != "combined") %>%
# pivot_wider(names_from = "cohort", values_from = "perc_total") %>%
# arrange(variable, category) %>%
# mutate(category = case_when(
# variable == "ethnicity" & category == 0 ~ "White",
# variable == "ethnicity" & category == 1 ~ "South Asian",
# variable == "ethnicity" & category == 2 ~ "Other",
# variable == "ethnicity" & category == "missing" ~ "Missing",
# variable == "sex" & category == 0 ~ "Male",
# variable == "sex" & category == 1 ~ "Female",
# variable == "sex" & category == "missing" ~ "Missing",
# variable == "pat_soc" & category == 0 ~ "class I or II",
# variable == "pat_soc" & category == 1 ~ "class III",
# variable == "pat_soc" & category == 2 ~ "class IV, V or other",
# variable == "pat_soc" & category == "missing" ~ "Missing",
# variable == "mat_ed" & category == 0 ~ "Left school at 15 or 16",
# variable == "mat_ed" & category == 1 ~ "Left school at 17 or 18",
# variable == "mat_ed" & category == 2 ~ "Degree",
# variable == "mat_ed" & category == "missing" ~ "Missing")) %>%
# mutate(across(where(is.numeric), as.character)) %>%
# mutate(variable = case_when(
# 	variable == "sex" ~ "Sex",
# 	variable == "ethnicity" ~ "Ethnicity",
# 	variable == "mat_ed" ~ "Maternal Education",
# 	variable == "pat_soc" ~ "Paternal occupation"))


### Median age at last measurement
# We can also calculate the median age at last measurement for each cohort.
# It isn't super straightforward in DataSHIELD, as functions aren't designed to
# show min and max values. To get round this, we can use the function
# "dh.makeOutcome" to convert the repeated measures weight data to a single
# variable, selecting the latest observation with subjects have >1 observation.
# ```{r alm}
# dh.makeOutcome(
# 	df = "data",
# 	outcome = "weight",
# 	age_var = "age",
# 	bands = c(0, 18),
# 	mult_action = "latest",
# 	df_name = "latest_meas",
# 	id_var = "id",
# 	band_action = "g_le")

# alm <- dh.getStats(
# 	df = "latest_meas",
# 	vars = "age.18")$continuous

# alm_out <- alm %>%
# select(cohort, perc_5, perc_50, perc_95) %>%
# mutate(iqr = round(perc_95 - perc_50, 2),
# 	   val_iqr = paste0(perc_50, " (", iqr, ")")) %>%
# select(cohort, val_iqr) %>%
# pivot_wider(
# 	names_from = cohort,
# 	values_from = val_iqr) %>%
# mutate(
# 	variable = "Median ALMc in years (IQR)",
# 	category = "")
}
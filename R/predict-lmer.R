dh.predictLmer <- function(model, newdata, coh_names){

## ---- First we add a column to the new data for the intercept ----------------
newdata <- newdata %>%
mutate(intercept = 1) %>%
select(intercept, everything())

## ---- First we extract coefficients ------------------------------------------
coefs <- dh.lmTab(
  model = model, 
  type = "lmer",
  coh_names = coh_names,
  direction = "long", 
  ci_format = "separate") 

## ---- Now we get the names of coefficients which aren't the intercept --------
coef_names <- coefs$fixed %>% 
pull(variable) %>%
unique

## ---- Now we get the coefficients for each cohort ----------------------------
coefs_by_cohort <- coefs$fixed %>%
pivot_wider(
	names_from = variable,
  values_from = value) %>%
dplyr::filter(coefficient == "est") %>%
select(-coefficient) %>%
group_by(cohort) 

## ---- Get the names of the groups (cohorts) ----------------------------------
coefs_by_cohort_names <- group_keys(coefs_by_cohort) %>% pull(cohort)

## ---- Make sure the columns are in the correct order -------------------------
coefs_by_cohort %<>% select(cohort, all_of(coef_names))
newdata_min <- newdata %>% select(all_of(coef_names))

## ---- Now we multiply the coefficients by new data ---------------------------
coefs_split <- coefs_by_cohort %>%
group_split(.keep = FALSE) %>%
set_names(coefs_by_cohort_names)

fixed <- coefs_split %>%
map(function(x){

newdata_min %>%
pmap_df(function(...){

out <- c(...)*x 
return(out)
})

})

## ---- Now do the business ----------------------------------------------------
pred <- fixed %>%
map(function(x){

newdata %>%
mutate(predicted = rowSums(x))

}) 


## ---- Now get the standard errors ------------------------------------------------
nstudy <- seq(1, model$num.valid.studies, 1)
study_ref <- paste0("study", nstudy)

## We get the vcov matrix for each study
vcov <- study_ref %>%
map(~model$output.summary[[.]]$vcov)

## Now we need to make sure our new data has the same order of columns as the
## vcov. This is slightly annoying because we had renamed our intercept 
## term to have a less silly name. We rename it again, get the columns in the 
## right order then put it back.

newdata <- newdata %>%
dplyr::rename("(Intercept)" = intercept) %>%
dplyr::select(colnames(vcov[[1]])) %>%
dplyr::rename(intercept = "(Intercept)")

## Feed in our newdata frame to get the SEs
se <- vcov %>% map(function(x){

newdata %>%
  pmap_dbl(function(...){

C <- c(...)
std.er <- sqrt(t(C) %*% x %*% C)
out <- std.er@x

return(out)
})

}) %>%
set_names(coh_names) %>%
map(as_tibble)

## Now currently we haven't found a way to compute standard errors for the pooled
## results. So as a placeholder we will create a list the same length as for each
## cohort and set that to NA.

comb_se <- list(
  combined = tibble(
    value = rep(NA, nrow(se[[1]]))
  )
)

se <- c(se, comb_se)

## ---- Join the standard errors back in with the predicted values ----------------
out <- names(pred) %>%
map(~bind_cols(pred[[.x]], se[[.x]])) %>%
map(~dplyr::rename(., se = value)) %>%
map(~mutate(., low_ci = predicted - 1.96 * se, 
         upper_ci = predicted + 1.96 * se)) %>%
set_names(names(pred)) %>%
bind_rows(.id = "cohort")

return(out)
}
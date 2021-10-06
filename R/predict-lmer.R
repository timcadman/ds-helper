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
coef_names <- coefs %>% 
pull(variable) %>%
unique

## ---- Now we get the coefficients for each cohort ----------------------------
coefs_by_cohort <- coefs %>%
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
newdata %<>% select(all_of(coef_names))

## ---- Now we multiply the coefficients by new data ---------------------------
coefs_split <- coefs_by_cohort %>%
group_split(.keep = FALSE) %>%
set_names(coefs_by_cohort_names)

fixed <- coefs_split %>%
map(function(x){

newdata %>%
pmap_df(function(...){

out <- c(...)*x 
return(out)
})

})

## ---- Now do the business ----------------------------------------------------
pred <- fixed %>%
map(function(x){

x %>%
mutate(predicted = rowSums(.))

}) %>%
bind_rows(.id = "cohort")

return(pred)
}
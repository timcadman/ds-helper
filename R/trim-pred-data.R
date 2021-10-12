dh.trimPredData <- function(pred, coh_names, min, max){

ref <- tibble(
	coh_ref = coh_names,
	min = min,
	max = max)

out <- ref %>%

pmap(function(coh_ref, min, max){
    
pred %>%
      filter(cohort == coh_ref & between(age, min, max))
  }) %>% 
  bind_rows()

  return(out)

} 
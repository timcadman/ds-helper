#' Produces descriptive statistics in useful format
#' 
#' This function extracts descriptive statistics from variables held in opal 
#' tables via DS. It mainly uses "ds.summary", but it also extracts extra 
#' info not given by default. It also avoids a problem encountered with 
#' ds.summary where it gets upset if the variable you request isn't present 
#' within a cohort. Instead this function just returns NA for that variable and 
#' for that cohort. This is more useful, e.g. if you want to make descriptive
#' tables for papers and show that a certain cohort is lacking some information.
#' Although, this may be less important if using ds.dataFrameFill throughout
#' your scripts.
#' 
#' @param df opal dataframe
#' @param vars vector of variable names in dataframe
#' 
#' @return The function returns a list with two elements containing dataframes 
#' with summary statistics for (i) categorical and (ii) continuous variables. 
#' These data frames are in longform and contain the following variables.
#' 
#' Categorical:
#' variable = variable
#'  category = level of variable
#'  value = number of observations
#'  cohort = name of cohort, including combined values for all cohorts
#'  cohort_n = total number of observations for cohort in dataset 
#'  valid_n = number of valid observations for variable (sum of ns for each 
#'            categories)
#'  missing_n = number of observations missing for variable (cohort_n - valid_n)
#'  valid_perc = observations within a category as percentage of valid_n 
#'  missing_perc = percentage of observations missing for a variable (valid_n / 
#'                 cohort_n)*100
#'
#' Continuous: 
#'
#'  cohort = cohort, including combined values for all cohorts
#'  variable = variable
#'  mean = mean (for combined value for all cohorts this is calculated by meta-
#'        analysis using fixed-effects)
#'  std.dev = standard deviation (again calculated by MA for cohorts combined)
#'  valid_n = as above
#'  cohort_n = as above
#'  missing_n = as above
#'  missing_perc = as above
#'  
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% arrange group_by group_map summarise summarize ungroup
#' @importFrom purrr map flatten_dbl
#' @importFrom dsBaseClient ds.class ds.summary ds.length ds.var
#' @importFrom stringr str_detect
#' @importFrom meta metamean
#' @importFrom stats setNames
#' @importFrom magrittr %<>%
#'  
#' @author Tim Cadman
#'  
#' @export
dh.getStats <- function(df, vars){
  
dh.doVarsExist(df, vars)
  
################################################################################
# 1. Identify variable type  
################################################################################  
  
## Create vector of full names for datashield
full_var_names <- paste0(df, "$", vars)

class_list <- full_var_names %>% map(ds.class, opals)

f <- class_list %>% map(function(x){any(str_detect(x, "factor") == TRUE)})
i <- class_list %>% map(function(x){any(str_detect(x, "numeric|integer") == TRUE)})

## Create separate vectors for factors and integers
factors <- vars[(which(f == TRUE))]
integers <- vars[(which(i == TRUE))]

## Create vector of opal names
cohorts <- names(opals)

################################################################################
# 2. Extract information using ds.summary
################################################################################

## ---- Categorical ------------------------------------------------------------
stats_cat <- list()
 
if(length(factors > 0)){
    
  stats_cat[[1]] <- lapply(factors, function(x){
      
      sapply(cohorts, USE.NAMES = FALSE, function(y){
        
        if(ds.length(paste0(df, "$", x), datasources = opals[y], 
                     type = "combine") == 0){
          
          list(NULL)
          
        } else{
          
          ds.summary(paste0(df, "$", x), datasources = opals[y])
          
        }
        
      })
    })
    
  stats_cat[[2]] <- ds.length(paste0(df, "$", factors[1]), type = "split", 
                          datasources = opals)
    
    names(stats_cat) <- c("Descriptives", "Max_N")
    names(stats_cat[[1]]) <- factors
    stats_cat[[1]] <- lapply(stats_cat[[1]], setNames, cohorts)
    names(stats_cat[[2]]) <- cohorts
    
}

## ---- Continuous -------------------------------------------------------------
stats_cont <- list()

if(length(integers > 0)){
    
  stats_cont[[1]] <- lapply(integers, function(x){
      
      sapply(cohorts, USE.NAMES = FALSE, function(y){
        
        if(ds.length(paste0(df, "$", x), datasources = opals[y], 
                     type = "combine") == 0){
          
          list(NULL)
          
        } else{
          
          ds.summary(paste0(df, "$", x), datasources = opals[y])
          
        }
        
      })
      
    })
    
    names(stats_cont[[1]]) <- integers
    stats_cont[[1]] <- lapply(stats_cont[[1]], setNames, cohorts)
    
    stats_cont[[2]] <- lapply(integers, function(x){
      
      sapply(cohorts, USE.NAMES = FALSE, function(y){
        
        if(ds.length(paste0(df, "$", x), datasources = opals[y], 
                     type = "combine") == 0){
          
          list(NULL)
          
        } else{
          
          ds.var(paste0(df, "$", x), datasources = opals[y])[1]
          
        }   
        
      })
      
    })
    
    names(stats_cont[[2]]) <- integers
    stats_cont[[2]] <- lapply(stats_cont[[2]], setNames, cohorts)
    
    lapply(stats_cont[[1]], names)
    
    stats_cont[[3]] <- ds.length(paste0(df, "$", integers[1]), type = "split", 
                          datasources = opals)
    
    names(stats_cont) <- c("Mean", "Variance", "Max_N")
    names(stats_cont[[3]]) <- cohorts
    
  }
  

################################################################################
# 3. Transform information into more usable format  
################################################################################

# Here we derive key information we will need for descriptives

## Create lists. This means that if there is no variable of that type an
## empty list can still be returned.

out_cat <- list()
out_cont <- list()


## ---- Categorical variables --------------------------------------------------

## Here we extract information from the lists we made above. I guess we could
## do it in one stage rather than two but this is how I conceptualise the 
## process.

## First we need to create a vector with repetitions of the variable names
## corresponding to the number of categories each variable has. This code isn't
## great but it works.

if(length(stats_cat) >0){

tmp <- map(stats_cat[[1]], function(x){
  map(cohorts, function(y){
    
    if(is.null(x[[y]])){
      NA
    } else{ 
      
      length(x[[y]]$categories)
    }
  })
})

cat_len <- map(tmp, function(x){
  
  len <- Reduce(`+`, x)
  
})

var_vec <- rep(names(cat_len), times = cat_len)

out_cat <- data.frame(
  variable = var_vec,
  category = unlist(
    map(stats_cat[[1]], function(x){
      map(cohorts, function(y){
        
        if(is.null(x[[y]])){
          NA
        } else{ 
          
            x[[y]]$categories
          }
      })
    }), use.names = FALSE
  ),
  value = unlist(
    map(stats_cat[[1]], function(x){
      map(cohorts, function(y){
        
        if(is.null(x[[y]])){
          NA
        } else{ 
          
          x[[y]][which(str_detect(names(x[[y]]), "count") == TRUE)]
          
        }
      })
    }), use.names = FALSE
  ), 
  cohort = unlist(
    map(stats_cat[[1]], function(x){
      map(cohorts, function(y){
        
        if(is.null(x[[y]])){
          y
        } else{ 
          
          rep(
            names(x[y]), 
            length = sum(str_detect(names(x[[y]]), "count") == TRUE)
          )
          
        }
      })
    }), use.names = FALSE
  ))


## Get total ns for each cohort
tmp <- map(out_cat$cohort, function(x){
  
  stats_cat[["Max_N"]][[match(x, names(stats_cat[["Max_N"]]))]]
  
})


## Combine these with df and convert to tibble
out_cat %<>%
  mutate(cohort_n = flatten_dbl(tmp)) %>%
  as_tibble()


## Calculate stats for all cohorts combined and add to tibble
all_sum <- out_cat %>%
  group_by(variable, category) %>%
  summarise(value = sum(value, na.rm = TRUE),
            cohort_n = sum(cohort_n, na.rm = TRUE)) %>%
  mutate(cohort = "combined") %>%
  ungroup()

out_cat <- rbind(out_cat, all_sum)


## Calculate additional stats 
out_cat %<>%
  group_by(cohort, variable) %>%
  mutate(valid_n = sum(value, na.rm = TRUE)) %>%
  ungroup()
  
out_cat %<>% mutate(
  missing_n = cohort_n - valid_n,
  valid_perc = round((value / valid_n) * 100, 2), 
  missing_perc = round((missing_n / cohort_n) * 100, 2))

}    
  

################################################################################
# Continuous variables  
################################################################################  
  
if(length(stats_cont) > 0){
    
out_cont <- data.frame(
  cohort = rep(cohorts, times = length(names(stats_cont[[1]]))), 
  variable = rep(names(stats_cont[[1]]), times = 1, each = length(cohorts)),
  mean = unlist(
    sapply(stats_cont[[1]], function(x){
      sapply(cohorts, simplify = FALSE, function(y){
            
            if(is.null(x[[y]])){
              NA
            } else{
              round(x[[y]]$"quantiles & mean"["Mean"], 2)
            }
            
          })
        })
      ),
      perc_25 = unlist(
        sapply(stats_cont[[1]], function(x){
          sapply(cohorts, simplify = FALSE, function(y){
            
            if(is.null(x[[y]])){
              NA
            } else{
              round(x[[y]]$"quantiles & mean"["25%"], 2)
            }
            
          })
        })
      ),
      perc_75 = unlist(
        sapply(stats_cont[[1]], function(x){
          sapply(cohorts, simplify = FALSE, function(y){
            
            if(is.null(x[[y]])){
              NA
            } else{
              round(x[[y]]$"quantiles & mean"["75%"], 2)
            }
            
          })
        })
      ),
      std.dev = unlist(
        sapply(stats_cont[[2]], function(x){
          sapply(cohorts, simplify = FALSE, function(y){
            
            if(is.null(x[[y]])){
              NA
            } else{
              round(sqrt(x[[y]][1]), 2)
            }
            
          })
        })
      ), 
      valid_n = unlist(
        sapply(stats_cont[[2]], function(x){
          sapply(cohorts, simplify = FALSE, function(y){
            
            if(is.null(x[[y]])){
              NA
            } else{
              x[[y]][3]
            }
            
          })
        })
      )
      
    )
    
    out_cont$cohort_n = unlist(
      apply(
        out_cont, 1, function(x){stats_cont[["Max_N"]][match(x["cohort"], names(stats_cont[["Max_N"]]))]})
    )
    
## We replace NAs in the "valid_n" column with 0
out_cont$valid_n[is.na(out_cont$valid_n)] <- 0
    
out_cont %<>% mutate(missing_n = cohort_n - valid_n)
    
## ---- Group output and pool means --------------------------------------------
    
## Need to sort df as group_by ends up sorting it and causing later trouble
out_cont %<>% arrange(variable)
    
pooled <- out_cont %>%
  group_by(variable) %>% 
  group_map(~ suppressWarnings(metamean(.x, n = .x$valid_n, mean = .x$mean, sd = .x$std.dev, studlab = cohorts)))
    
coh_comb <- data.frame(
  cohort = "Combined",
  variable = sort(names(stats_cont[[1]])),
  mean = round(unlist(sapply(pooled, function (x){x["TE.fixed"]})), 2),
  se = unlist(sapply(pooled, function (x){x["seTE.fixed"]})),
  valid_n = out_cont %>% group_by(variable) %>% 
    dplyr::summarize(valid_n = sum(valid_n, na.rm = TRUE)) %>% .[, 2],
  cohort_n = Reduce(`+`, stats_cont[["Max_N"]]))

coh_comb %<>% 
  mutate(
    missing_n = cohort_n - valid_n,
    std.dev = round(se * sqrt(valid_n), 2)) %>% 
  select(-se)
  
    
## ---- Combine with main table ------------------------------------------------
    
out_cont %<>% select(-perc_25, -perc_75)
out_cont <- rbind(out_cont, coh_comb)
    
out_cont %<>% 
  mutate(missing_perc = round((missing_n / cohort_n) * 100, 2)) %>%
  as_tibble()
    
}
out <- list(out_cat, out_cont)
names(out) <- c("categorical", "continuous")

return(out)

}
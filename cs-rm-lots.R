################################################################################
## Project: ds-cs-functions
## Script purpose: Allow use of ds.rm with more than one variable
## Date: 5th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################        

# This is a very simple wrapper around ds.rm to allow you to remove more than
# one object in one function call.
#
# Arguments:
#
# vars = objects that you want to remove
#
# Value: None. Objects remove from opal environment

require(purrr)
require(dplyr)

cs.rmLots <- function(vars){
  
  vars %>% map(ds.rm)
  
}
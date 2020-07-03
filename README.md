# DataSHIELD helper functions

[Build status](https://travis-ci.org/lifecycle-project/ds-cs-functions.svg?branch=master)

This is a set of functions to automate processes in DataSHIELD to make data
manipulation and analysis easier. They are very much a work in progress! If you 
have written functions that would also be useful to other DS users drop me an 
email and we can incorporate them in the package. 

## Install
You can install the package by running the following command in R Studio:

    install.packages("remotes")
    library(remotes)
    install_github("lifecycle-project/ds-cs-functions", ref="makepackage")
    library(dsHelper)
    
## Notes
Currently these functions require your opal object is labelled "opals". If 
required I can add functionality for users to specify a different name.

## Development
If you find any bugs, would like to request additional functionality or have
written functions you would like to be included in the package please contact me 
at t.cadman@bristol.ac.uk. 

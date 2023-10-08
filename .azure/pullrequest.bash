#!/bin/bash
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install())"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check(force_suggests = TRUE))"
#Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', library(covr);codecov())"
#Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', library(lintr);lintr::lint_package(linters=lintr::with_defaults(object_name_linter(styles = 'dotted.case'), line_length_linter(120))))"
#Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', quit(save = 'no', status = length(lintr::lint_package(linters=lintr::with_defaults(object_name_linter(styles = 'dotted.case'), line_length_linter(120))))))"

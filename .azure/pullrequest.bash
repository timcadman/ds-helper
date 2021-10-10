#!/bin/bash
RELEASE_SCOPE="patch"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', usethis::use_version('${RELEASE_SCOPE}'))"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install())"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check(remote=TRUE, force_suggests = TRUE))"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', lintr::lint_package())"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', quit(save = 'no', status = length(lintr::lint_package()))"
#Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', covr::codecov())"
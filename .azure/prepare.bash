#!/bin/bash

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"

Rscript -e "install.packages(c('git2r', 'covr', 'withr', 'devtools', 'lintr', 'mockery', 'pkgdown'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('DSI', 'metafor', 'meta'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install_github('datashield/dsBaseClient', ref='v6.2-dev'))"
cd "${BUILD_REPOSITORY_LOCALPATH}"
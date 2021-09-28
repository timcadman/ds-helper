#!/bin/bash

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"

Rscript -e "install.packages(c('covr', 'withr', 'devtools', 'lintr', 'mockery'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('dsBaseClient', 'DSI', 'metafor', 'meta'), repos=c('https://cloud.r-project.org','https://cran.obiba.org'), lib='${R_LIBS_USER}')"

cd "${BUILD_REPOSITORY_LOCALPATH}"
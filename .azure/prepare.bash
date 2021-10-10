#!/bin/bash

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"

git config --global user.email "sido@haakma.org"
git config --global user.name "Azure Pipeline"
git remote set-url origin "https://${GITHUB_TOKEN}@github.com/lifecycle-project/ds-helper.git"

Rscript -e "install.packages(c('covr', 'withr', 'devtools', 'lintr', 'mockery'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('DSI', 'metafor', 'meta'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "library(devtools); install_github('datashield/dsBaseClient', ref='6.1.0')"
cd "${BUILD_REPOSITORY_LOCALPATH}"
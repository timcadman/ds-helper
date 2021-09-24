#!/bin/bash
# Azure Pipeline predefined environment variables
# - BUILD_REASON: we check on "PullRequest"
# - BUILD_REPOSITORY_LOCALPATH: we need go back to the directory that contains the sources after the loop
# - AGENT_HOMEDIRECTORY: we need this to store the docker credentials
# - SYSTEM_PULLREQUEST_PULLREQUESTID: PullRequestID from GitHub
# - SYSTEM_PULLREQUEST_TARGETBRANCH: PullRequest target branch e.g. main
# Additional environment variables to make sure the release works
# - DOCKERHUB_AUTH: we use this credential to push the dockers to the registry
# - GITHUB_TOKEN: semantic release uses this environment variable to push to github

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)

apt-get update && apt-get install libharfbuzz-dev libfribidi-dev -y
Rscript -e "install.packages(c('git2r', 'usethis', 'devtools', 'pkgdown', 'mockery'), repos='https://cloud.r-project.org')"
Rscript -e "install.packages(c('dsBaseClient', 'DSI', 'metafor'), repos=c('https://cloud.r-project.org','https://cran.obiba.org'))"
Rscript -e "git2r::config(user.email = 'sido@haakma.org', user.name = 'Sido Haakma')"

cd "${BUILD_REPOSITORY_LOCALPATH}"
if [[ "${BUILD_REASON}" == "PullRequest" ]] 
then     
  echo "Building R-package: [ ${BUILD_REPOSTIORY_NAME} ]"  
  Rscript -e 'devtools::install()'
  Rscript -e 'devtools::check(remote=TRUE, force_suggests = TRUE)'
  Rscript -e 'usethis::use_tidy_style()'
  Rscript -e 'library(covr);codecov()'
else
  if [[ ! "${BUILD_SOURCEVERSIONMESSAGE}" =~ "[ci skip]" ]]
  then
    RELEASE_SCOPE="patch"
    git checkout master
    Rscript -e "usethis::use_version('${RELEASE_SCOPE}'')"
    TAG=$(grep Version DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
    PACKAGE=$(grep Package DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
    git commit -a -m "[ci skip] Created release: ${TAG}"
    echo "Releasing ${PACKAGE} ${TAG}"
    R CMD build .
    Rscript -e "devtools::check_built(path = './${PACKAGE}_${TAG}.tar.gz', remote=TRUE, force_suggests = TRUE)"
    #set +x; curl -v --user "${NEXUS_USER}:${NEXUS_PASS}" --upload-file "${PACKAGE}_${TAG}".tar.gz "${REGISTRY}"/src/contrib/"${PACKAGE}_${TAG}".tar.gz
    git tag "${TAG}"
    git push origin "${TAG}"
    echo "Creating new development version"
    Rscript -e "usethis::use_version('dev')"
    git commit -a -m '[ci skip]: Increment dev-version number'
    git push origin master
  fi
fi

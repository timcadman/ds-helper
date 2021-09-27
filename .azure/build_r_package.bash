#!/bin/bash
# Azure Pipeline predefined environment variables
# - BUILD_REASON: we check on "PullRequest
# - BUILD_REPOSTIORY_NAME: we use to determine the package that is build
# - BUILD_REPOSITORY_LOCALPATH: we need go back to the directory that contains the sources after the loop
# - AGENT_HOMEDIRECTORY: we need this to store the docker credentials
# - SYSTEM_PULLREQUEST_PULLREQUESTID: PullRequestID from GitHub
# - SYSTEM_PULLREQUEST_TARGETBRANCH: PullRequest target branch e.g. main
# Additional environment variables to make sure the release works
# - GITHUB_TOKEN: semantic release uses this environment variable to push to github
# - NEXUS_USER: repository username
# - NEXUS_PASS: repository password
# - REGISTRY: repository url
# - R_LIBS_USER: home directory user libraries
# - CODECOV_TOKEN: token to authenticate to codecov

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"

Rscript -e "install.packages(c('covr', 'git2r', 'withr', 'devtools', 'lintr', 'mockery'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('dsBaseClient', 'DSI', 'metafor', 'meta'), repos=c('https://cloud.r-project.org','https://cran.obiba.org'), lib='${R_LIBS_USER}')"
Rscript -e "git2r::config(user.email = 'sido@haakma.org', user.name = 'Sido Haakma')"

cd "${BUILD_REPOSITORY_LOCALPATH}"
if [[ "${BUILD_REASON}" == "PullRequest" ]] 
then     
  echo "Build PR for R-package: [ ${BUILD_REPOSITORY_NAME} ]"  
  Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install())"
  Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check(remote=TRUE, force_suggests = TRUE))"
  Rscript -e "quit(save = 'no', status = length(lintr::lint_package()))"
  Rscript -e 'covr::codecov()'
else
  if [[ ! "${BUILD_SOURCEVERSIONMESSAGE}" =~ "[ci skip]" ]]
  then
    echo "Release R-package: [ ${BUILD_REPOSITORY_NAME} ]"  
    RELEASE_SCOPE="patch"
    git checkout master
    Rscript -e "usethis::use_version('${RELEASE_SCOPE}')"
    TAG=$(grep Version DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
    PACKAGE=$(grep Package DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
    git commit -a -m "[ci skip] Created release: ${TAG}"
    echo "Releasing ${PACKAGE} ${TAG}"
    R CMD build .
    Rscript -e "devtools::check_built(path = './${PACKAGE}_${TAG}.tar.gz', remote=TRUE, force_suggests = TRUE)"
    #set +x; curl -v --user "${NEXUS_USER}:${NEXUS_PASS}" --upload-file "${PACKAGE}_${TAG}".tar.gz "${REGISTRY}"/src/contrib/"${PACKAGE}_${TAG}".tar.gz
    git tag "${TAG}"
    git push origin "${TAG}"
    echo "Creating new development version for R-package: [ ${BUILD_REPOSITORY_NAME} ]"
    Rscript -e "usethis::use_version('dev')"
    git commit -a -m '[ci skip]: Increment dev-version number'
    git push origin master
  else
    echo "Skip CI for R-package: [ ${BUILD_REPOSITORY_NAME} ]"
  fi
fi

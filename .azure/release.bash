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

RELEASE_SCOPE="patch"
git checkout master
git remote set-url origin https://${GITHUB_TOKEN}@github.com/lifecycle-project/ds-helper.git
Rscript -e "usethis::use_version('${RELEASE_SCOPE}')"
TAG=$(grep Version DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
PACKAGE=$(grep Package DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
git commit -a -m "[ci skip] Created release: ${TAG}"
echo "Releasing ${PACKAGE} ${TAG}"
R CMD build . --library ${R_USER_LIBS}
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check_built(path = './${PACKAGE}_${TAG}.tar.gz', remote=TRUE, force_suggests = TRUE))"
#set +x; curl -v --user "${NEXUS_USER}:${NEXUS_PASS}" --upload-file "${PACKAGE}_${TAG}".tar.gz "${REGISTRY}"/src/contrib/"${PACKAGE}_${TAG}".tar.gz
git tag "${TAG}"
git push origin "${TAG}"
echo "Creating new development version for R-package: [ ${BUILD_REPOSITORY_NAME} ]"
Rscript -e "usethis::use_version('dev')"
git commit -a -m '[ci skip]: Increment dev-version number'
git push origin master
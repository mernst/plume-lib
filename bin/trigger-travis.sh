#!/bin/sh -f

# Trigger a new Travis-CI job.
# Usage:
#   trigger-travis.sh GITHUBID GITHUBPROJECT TRAVIS_ACCESS_TOKEN [MESSAGE]

# To use this script to trigger a dependent build in Travis, do two things:
#
# 1. Set an environment variable TRAVIS_ACCESS_TOKEN by navigating to
#   https://travis-ci.org/MYGITHUBID/MYGITHUBPROJECT/settings
# Determine the value for TRAVIS_ACCESS_TOKEN via: travis login && travis token
# (You may need to first do:
#    sudo apt-get install ruby-dev && sudo gem install travis
# Don't do "sudo apt-get install travis" which installs a trajectory analyzer.)
# This differs from the token available at https://travis-ci.org/profile .
# The environment variable will be set when Travis runs the job,
# but won't be visible to anyone browsing https://travis-ci.org/.
#
# 2. Add the following after_success block to your .travis.yml file,
# where you replace OTHERGITHUB* by a specific downstream project,
# but you leave $TRAVIS_ACCESS_TOKEN as literal text:
#
# after_success:
#   - |
#       if [[ ($TRAVIS_BRANCH == master) &&
#             ($TRAVIS_PULL_REQUEST == false) &&
#             ( (! $TRAVIS_JOB_NUMBER == *.*) || ($TRAVIS_JOB_NUMBER == *.1) ) ]] ; then
#         curl -LO https://raw.github.com/mernst/plume-lib/master/bin/trigger-travis.sh
#         sh trigger-travis.sh OTHERGITHUBID OTHERGITHUBPROJECT $TRAVIS_ACCESS_TOKEN
#       fi
#
# Note that Travis does not fail a job if an after_success command fails.
# If you misspell a GitHub ID or project name, then this script will fail,
# but Travis won't inform you of the mistake.  So, check the end of the
# Travis buid log the first time that a build succeeds.

# Here is an explanation of the conditional in the after_success block:
#
# 1. Downstream projects are triggered only for builds of the mainline, not
# branches or pull requests.  The reason is that typically a downstream
# project clones and uses the mainline.  You could enhance this script to
# accept pass an environment variable for the upstream project; the
# downstream project's build script would need to read and use that
# environment variable.  If you make this enhancement, feel free to submit
# a pull request so that others can benefit from it.
#
# 2. Downstream projects are triggered only if the Travis job number
# contains no "." or ends with ".1".  In other words, if your .travis.yml
# defines a build matrix
# (https://docs.travis-ci.com/user/customizing-the-build/#Build-Matrix)
# that runs the same job using different configurations, then the
# "after_success:" block is run only for the first configuration.  By
# default an after_success: block is run for every build in the matrix, but
# you really want it to run once if all the builds in the matrix succeed.
# For a workaround, see https://github.com/dmakhno/travis_after_all , but I
# couldn't get its permissions to work and don't know why.  The given test
# is a hack, because the downstream job is triggered even if some job other
# than the first one fails.  However, the given test is simple and it is
# usually adequate.

# An alternative to this script would be to install the Travis command-line
# client and then run:
#   travis restart -r OTHERGITHUBID/OTHERGITHUBPROJECT
# That is undesirable because it restarts an old job, destroying its history,
# rather than starting a new job which is our goal.

# Parts of this script were originally taken from
# http://docs.travis-ci.com/user/triggering-builds/

USER=$1
REPO=$2
TOKEN=$3
if [ $# -eq 4 ] ; then
    MESSAGE=",\"message\": \"$4\""
elif [ -n "$TRAVIS_REPO_SLUG" ] ; then
    MESSAGE=",\"message\": \"Triggered by upstream build of $TRAVIS_REPO_SLUG commit "`git rev-parse --short HEAD`"\""
else
    MESSAGE=""
fi
## For debugging:
# echo "MESSAGE=$MESSAGE"

body="{
\"request\": {
  \"branch\":\"master\"
  $MESSAGE
}}"

# It does not work to put / in place of %2F in the URL below.  I'm not sure why.
curl -s -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token ${TOKEN}" \
  -d "$body" \
  https://api.travis-ci.org/repo/${USER}%2F${REPO}/requests \
 | tee /tmp/travis-request-output.$$.txt

if grep -q '"@type": "error"' /tmp/travis-request-output.$$.txt; then
    exit 1
fi
if grep -q 'access denied' /tmp/travis-request-output.$$.txt; then
    exit 1
fi

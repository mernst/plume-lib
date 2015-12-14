#!/bin/sh -f

# Trigger a new Travis-CI job.
# Useful for triggering a dependent build in Travis.
# Usage:
#   trigger-travis.sh GITHUBID GITHUBPROJECT TRAVISTOKEN [MESSAGE]

# To use this script, do two things:
# 
# 1. Set an environment variable TRAVISTOKEN by navigating to
#   https://travis-ci.org/MYGITHUBID/MYGITHUBPROJECT/settings
# The environment variable will be set when Travis runs the job,
# but won't be visible to anyone browsing https://travis-ci.org/.
# Determine the value for TRAVISTOKEN via:  travis login && travis token
# (You may need to first do:
#    sudo apt-get install ruby-dev && sudo gem install travis
# but don't do "sudo apt-get install travis"; use the above instead.)
# This differs from the token available at https://travis-ci.org/profile .
# 
# 2. Add the following after-success block to your .travis.yml file,
# where you replace OTHERGITHUB* by a specific downstream project,
# but you leave $TRAVISTOKEN and $TRAVIS_REPO_SLUG as literal text:
#
# after-success:
#   - curl -LO https://raw.github.com/mernst/plume-lib/master/bin/trigger-travis.sh
#   - sh trigger-travis.sh OTHERGITHUBID OTHERGITHUBPROJECT $TRAVISTOKEN "Triggered by upstream build of $TRAVIS_REPO_SLUG"

# There are two caveats to calling this in the "after-success:" block.
#
# 1. Travis does not fail a job if an after-success command fails.  If you
# misspell a GitHub ID or project name, then Travis won't inform you of
# this mistake.  So, check the end of the Travis buid log the first
# time that a build of THISGITHUBID/THISGITHUBPROJECT succeeds.
#
# 2. The "after-success:" block is run once for every matrix build, but you
# only want it to run once if all the builds in the matrix succeed.  For
# a workaround, see https://github.com/dmakhno/travis_after_all .  You would
# write in your .travis.yml file:
#
# after-success:
#   - curl -LO https://raw.github.com/dmakhno/travis_after_all/master/travis_after_all.py
#   - python travis_after_all.py
#   - export $(cat .to_export_back)
#   # the vertical bar (pipe) means multi-line command in YAML
#   - |
#       if [ "$BUILD_LEADER" = "YES" ] && [ "$BUILD_AGGREGATE_STATUS" = "others_succeeded" ]; then
#         curl -LO https://raw.github.com/mernst/plume-lib/master/bin/trigger-travis.sh
#         sh trigger-travis.sh OTHERGITHUBID OTHERGITHUBPROJECT $TRAVISTOKEN "Triggered by upstream build of $TRAVIS_REPO_SLUG"
#       fi


# An alternative to this script would be to install the Travis command-line
# client and then run:
#   travis restart -r OTHERGITHUBID/OTHERGITHUBPROJECT
# That is undesirable because it restarts an old job, making its history
# hard to see, rather than starting a new job which is our goal.

# This script was originally taken from
# http://docs.travis-ci.com/user/triggering-builds/

USER=$1
REPO=$2
TOKEN=$3
if [ $# -eq 4 ] ; then
    MESSAGE=",\"message\": \"$4\""
else
    MESSAGE=""
fi

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

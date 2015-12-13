#!/bin/sh -f

# Trigger a new Travis-CI job.
# Useful for triggering a dependent build in Travis.

# To use this script, do two things:
# 
# 1. Set an environment variable TRAVISTOKEN by navigating to
#   https://travis-ci.org/MYGITHUBID/MYGITHUBPROJECT/settings
# The environment variable will be set when Travis runs the job,
# but won't be visible to anyone even though the repository is public.
# Determine the value for TRAVISTOKEN via:  travis login && travis token
# (You may need to first do:
#    sudo apt-get install ruby-dev && sudo gem install travis
# but don't do "sudo apt-get install travis"; use the above instead.)
# This differs from the token available at https://travis-ci.org/profile .
# 
# 1. Add the following after-success block to your .travis.yml file,
# where you replace MYGITHUB* by a specific downstream project
# but you leave $TRAVISTOKEN as literal text:
#
# after-success:
#   - curl -s https://raw.githubusercontent.com/mernst/plume-lib/master/bin/trigger-travis.sh > trigger-travis.sh
#   - sh trigger-travis.sh MYGITHUBID MYGITHUBPROJECT $TRAVISTOKEN

# An alternative to this script would be to install the Travis command-line
# client and then run,
#   travis restart -r MYGITHUBID/MYGITHUBPROJECT
# That is undesirable because it deletes an old job and loses its history,
# rather than starting a new job which is our goal.

# This script was originally taken from
# http://docs.travis-ci.com/user/triggering-builds/

USER=$1
REPO=$2
TOKEN=$3

body='{
"request": {
  "branch":"master"
}}'

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

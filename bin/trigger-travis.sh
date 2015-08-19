#!/bin/sh -f

# Trigger a new Travis-CI job

# Add one of the following after-success blocks to your .travis.yml file:
#
# after-success:
#   - trigger-travis.sh MYGITHUBID MYGITHUBPROJECT $MYTRAVISTOKEN
#
# after-success:
#   - echo Triggering build of MYGITHUBID/MYGITHUBPROJECT
#   - curl -s https://raw.githubusercontent.com/mernst/plume-lib/master/bin/trigger-travis.sh > trigger-travis.sh
#   - bash trigger-travis.sh MYGITHUBID MYGITHUBPROJECT $TRAVISTOKEN
#   - rm trigger-travis.sh
#
# and also set an environment variable TRAVISTOKEN by navigating to
# https://travis-ci.org/MYGITHUBID/MYGITHUBPROJECT/settings
# Determine the value of TRAVISTOKEN via:  travis login && travis token
# You may need to first do:  sudo apt-get install ruby-dev && gem install travis
# This differs from the token available at https://travis-ci.org/profile

# An alternative to this script would be to install the Travis command-line
# client and then run, for example,
#   travis restart -r mernst/plume-lib
# That is undesirable because it deletes an old job and loses its history,
# rather than starting a new job which is our goal.

# This script was originally taken from http://docs.travis-ci.com/user/triggering-builds/

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
  https://api.travis-ci.org/repo/${USER}%2F${REPO}/requests

#!/bin/sh -f

# Trigger a new Travis-CI job

# To use this, do two things:
# 
# 1. Set an environment variable TRAVISTOKEN by navigating to
#   https://travis-ci.org/MYGITHUBID/MYGITHUBPROJECT/settings
# The environment variable will be set when Travis runs the job,
# but won't be visible to anyone even though the repository is public.
# Determine the value to set TRAVISTOKEN to via:  travis login && travis token
# (You may need to first:  sudo apt-get install ruby-dev && sudo gem install travis
# Don't do "sudo apt-get install travis"; use the above instead.)
# This differs from the token available at https://travis-ci.org/profile .
# 
# 1. Add one of the following after-success blocks to your .travis.yml file,
# where $TRAVISTOKEN is literal text but MYGITHUB* should be replaced:
#
# after-success:
#   - trigger-travis.sh MYGITHUBID MYGITHUBPROJECT $TRAVISTOKEN
#
# after-success:
#   - curl -s https://raw.githubusercontent.com/mernst/plume-lib/master/bin/trigger-travis.sh > trigger-travis.sh
#   - sh trigger-travis.sh MYGITHUBID MYGITHUBPROJECT $TRAVISTOKEN
#
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
  https://api.travis-ci.org/repo/${USER}%2F${REPO}/requests

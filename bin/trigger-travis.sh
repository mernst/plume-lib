#!/bin/sh -f

# Trigger a new Travis-CI job
# Add something the following section to your .travis.yml file:
#
# after-success:
#   - trigger-travis.sh MYGITHUBID MYGITHUBPROJECT MYTRAVISTOKEN
#
# Determine MYTRAVISTOKEN via:  travis login && travis token
# You may need to first do:  sudo apt-get install ruby-dev && gem install travis
# This differs from the token available at https://travis-ci.org/profile


# Question: is there a way to do this without putting the token in the
# travis.yml file?  According to
# http://blog.travis-ci.com/2013-01-28-token-token-token/, "it's not super
# secret", but nonetheless it would be preferable not to include it in the
# publicly-visible .travis.yaml file.

# An alternative would be to install the Travis command-line client and
# then run, for example,
#   travis restart -r mernst/plume-lib
# That is undesirable because it deletes an old job and loses its history,
# rather than starting a new one.

# This script was taken from http://docs.travis-ci.com/user/triggering-builds/

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

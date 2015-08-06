#!/bin/sh -f

# Trigger a new Travis-CI job
# Add the following section to your .travis.yml file:
#
# after-success:
#   - trigger-travis.sh mernst randoop JajuCi2u6gf8gABmQPAdxg


# Question: is there a way to do this without putting the token in the
# travis.yml file?  According to
# http://blog.travis-ci.com/2013-01-28-token-token-token/, "it's not super
# secret", but it would still be nice not to include it.


# From http://docs.travis-ci.com/user/triggering-builds/

# An alternative would be to install the Travis command-line client and
# then run, for example,
#   travis restart -r mernst/plume-lib
# That is undesirable because it deletes an old job and loses its history,
# rather than starting a new one.


USER=$1
REPO=$2
# This is *not* the token available at https://travis-ci.org/profile
# Determine the token via: sudo apt-get install ruby-dev && gem install travis && travis login && travis token
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

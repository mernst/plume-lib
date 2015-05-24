#!/usr/bin/env bash

# Alias for git. Runs git and if there is a conflict, also runs "git mergetool".
# Needs an alias in your ~/.bashrc, but ONLY for non-interactive shells (to
# avoid infinite recursion), as follows:
#   if [ ! -z "$PS1" ]; then
#     alias git=git-auto-invoke-mergetool.sh
#   fi

# From http://stackoverflow.com/questions/10032265, with improvements.

# TODO: Improve the grep patterns.

if `git "$@" 2>&1 | tee /dev/tty | grep -q "^CONFLICT.*: Merge conflict in \|^Pull is not possible because you have unmerged files." 1>/dev/null 2>&1`
then
  git mergetool
fi

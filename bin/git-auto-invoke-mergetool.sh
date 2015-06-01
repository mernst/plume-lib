#!/usr/bin/env bash

# Alias for git. Runs git and if there is a conflict, also runs "git mergetool".
# Needs an alias in your ~/.bashrc, but ONLY for non-interactive shells (to
# avoid infinite recursion), as follows:
#   if [ ! -z "$PS1" ]; then
#     alias git=git-auto-invoke-mergetool.sh
#   fi

# A problem with this is that redirection does not work properly.
# For example, if you do
#   git some-git-command > stdout.txt 2>stderr.txt
# from the command line, you will still see output to /dev/tty.
# Although this script is intended only for interactive use, that is still
# a show-stopper problem.
# Possible solutions at:
# http://stackoverflow.com/questions/2342826/how-to-pipe-stderr-and-not-stdout
# http://unix.stackexchange.com/questions/3514/how-to-grep-standard-error-stream-stderr
# http://www.burgerbum.com/stderr_pipe.html
# http://stackoverflow.com/questions/12517519/how-to-redirect-stdoutstderr-to-one-file-while-keeping-streams-separate
# http://stackoverflow.com/questions/3173131/redirect-copy-of-stdout-to-log-file-from-within-bash-script-itself
# http://unix.stackexchange.com/questions/157689/how-to-capture-ordered-stdout-stderr-and-add-timestamp-prefixes
# http://urbanautomaton.com/blog/2014/09/09/redirecting-bash-script-output-to-syslog/

# This script is originally from http://stackoverflow.com/questions/10032265 .
# I have made improvements, such as to the grep patterns and the documentation.

# TODO: Improve the grep patterns.

if `git "$@" 2>&1 | tee /dev/tty | grep -q "^CONFLICT.*: Merge conflict in \|^Pull is not possible because you have unmerged files." 1>/dev/null 2>&1`
then
  git mergetool
fi

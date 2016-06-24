#!/usr/bin/python

# This script reformats each file supplied on the command line according to
# the Google Java style (by calling out to the google-java-format program,
# https://github.com/google/google-java-format), but with improvements to
# the formatting of annotations in comments.

import filecmp
import os
import stat
import subprocess
import sys
import tempfile
import urllib

debug = False
# debug = True

script_dir = os.path.dirname(os.path.abspath(__file__))
# Rather than calling out to the shell, it would be better to
# call directly in Python.
fixup_py = os.path.join(script_dir, "fixup-google-java-format.py")

gjf_jar_name = "google-java-format-1.0-all-deps.jar"
# Set gjf_jar_path
if os.path.isfile(os.path.join(script_dir, gjf_jar_name)):
    gjf_jar_path = os.path.join(script_dir, gjf_jar_name)
elif os.path.isfile(os.path.join(os.path.dirname(script_dir), "lib", gjf_jar_name)):
    gjf_jar_path = os.path.join(os.path.dirname(script_dir), "lib", gjf_jar_name)
else:
    gjf_jar_path = os.path.join(script_dir, gjf_jar_name)
    urllib.urlretrieve("https://github.com/google/google-java-format/releases/download/google-java-format-1.0/google-java-format-1.0-all-deps.jar", gjf_jar_path)

if not os.path.isfile(fixup_py):
    # TODO: do all this in check-google-java-format.py, too.
    # TODO: should replace local file if remote it is more recent, but don't fail if no network connection exists.
    # Could use: http://stackoverflow.com/questions/31105606/downloading-files-based-on-timestamp-in-python
    # or http://superuser.com/questions/1049202/curl-check-if-file-is-newer-and-instead-of-downloading-execute-a-bash-or-pyth
    urllib.urlretrieve("https://raw.githubusercontent.com/mernst/plume-lib/master/bin/fixup-google-java-format.py", fixup_py)
    # Make fixup_py executable by the user, like "chmod +x"
    os.chmod(fixup_py, os.stat(fixup_py).st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

if debug:
    print("script_dir:", script_dir)
    print("fixup_py: ", fixup_py)
    print("gjf_jar_path: ", gjf_jar_path)

files = sys.argv[1:]
if len(files) == 0:
    print("run-google-java-format.py expects 1 or more filenames as arguments")
    sys.exit(1)

result = subprocess.call(["java", "-jar", gjf_jar_path, "--replace", "--sort-imports=also"] + files)
# Don't stop if there was an error, because google-java-format won't munge
# files and we still want to run fixup-google-java-format.py.
# if result != 0:
#     print("Error when running google-java-format")
#     sys.exit(result)

files = [f for f in files if not f.startswith("--")]
# Exit if no files were supplied (maybe "--help" was)
if not files:
    sys.exit(0)

if debug: print("Running fixup-google-java-format.py")
result = subprocess.call([fixup_py] + files)
if result != 0:
    print("Error when running fixup-google-java-format.py")
    sys.exit(result)


###########################################################################
### end of script
###

# If you reformat your codebase, then that may be disruptive to people who
# have changes in their own branches/clones/forks.  (But, once you settle
# on consistent formatting, that will never be a problem again.)

# Here are some notes about a possible way to deal with upstream
# reformatting, which have not yet been tested by fire:

# For the person doing the reformatting:
#  * Tag the commit before the whitespace change as "before reformatting".
#     git tag -a before-reformatting -m "Code before running google-java-format"
#  * Reformat by running a command such as:
#     make reformat
#     ant reformat
#     gradle googleJavaFormat
#  * Examine the diffs:
#     git diff -w -b | grep -v '^[-+]import' | grep -v '^[-+]$'
#    Run a tool that seaches the diffs for hunks that have fewer lines than
#    they were before.  These are possibly places where an if/for/while whose
#    body was a single statement that has gotten sucked up onto the if/for/while
#    loop.  Or, just run a tool that searches the diffs for if/for/while with
#    body on the same line.  Add curly braces to get the body back on its own
#    line. 
#  * Tag the commit that does the whitespace change as "after reformatting".
#    git tag -a after-reformatting -m "Code after running google-java-format"
# 
# For a client to merge the massive upstream changes:
#  * Merge in the commit before the reformatting into your branch.
#  * Merge in the reformatting commit, preferring all your own changes.
#  * Run "ant reformat" or the equivalent command.
#  * Commit your changes.

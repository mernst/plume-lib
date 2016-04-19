#!/usr/bin/python

# This script checks whether the files supplied on the command line conform
# to the Google Java style (as enforced by the google-java-format program).
# If any files would be affected by running google-java-format, this script
# prints their names and returns a non-zero status.
# If called with no arguments, it reads from standard output.

import os
import re
import sys
import tempfile
import filecmp
import subprocess
import urllib

debug = False

script_dir = os.path.dirname(os.path.abspath(__file__))
# Rather than calling out to the shell, it would be better to
# call directly in Python
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
    urllib.urlretrieve("https://raw.githubusercontent.com/mernst/plume-lib/master/bin/fixup-google-java-format.py", fixup_py)


defult_tmp_dir = tempfile._get_default_tempdir()

exit_code = 0

def check_file(infile_name):
    """Fix up formatting while reading from infile_name and writing to outfile."""

    temp_name = next(tempfile._get_candidate_names())

    with open(temp_name,'w') as outfile:
        result = subprocess.call(["java", "-jar", gjf_jar_path, infile_name],
                                 stdout=outfile)
        if result != 0:
            sys.exit(result)
    result = subprocess.call([fixup_py, temp_name])
    if result != 0:
        sys.exit(result)
    if not filecmp.cmp(infile_name, temp_name):
        # TODO: gives temporary file name if reading from stdin
        print "Improper formatting:", infile_name
        exit_code = 1

if len(sys.argv) == 1:
    content = sys.stdin.read()
    fname = next(tempfile._get_candidate_names())
    with open(fname,'w') as outfile:
        print >>outfile, content
    check_file(fname)
    os.remove(fname)
else:
    for fname in sys.argv[1:]:
        check_file(fname)

sys.exit(exit_code)

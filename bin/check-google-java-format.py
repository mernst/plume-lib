#!/usr/bin/python

# This script checks whether the files supplied on the command line conform
# to the Google Java style (as enforced by the google-java-format program,
# but with improvements to the formatting of annotations in comments).
# If any files would be affected by running run-google-java-format.py,
# this script prints their names and returns a non-zero status.
# If called with no arguments, it reads from standard input.
# You could invoke this program, for example, in a git pre-commit hook.

# Here are example targets you might put in a Makefile; integration with
# other build systhems is similar.
#
# reformat:
# 	@wget -N https://raw.githubusercontent.com/mernst/plume-lib/master/bin/run-google-java-format.py
# 	@../plume-lib/bin/run-google-java-format.py ${JAVA_FILES_FOR_FORMAT}
#
# check-format:
# 	@wget -N https://raw.githubusercontent.com/mernst/plume-lib/master/bin/check-google-java-format.py
# 	@../plume-lib/bin/check-google-java-format.py ${JAVA_FILES_FOR_FORMAT} || (echo "Try running:  make reformat" && false)

from __future__ import print_function

import os
import re
import shutil
import sys
import tempfile
import filecmp
import subprocess
import urllib

debug = False

script_dir = os.path.dirname(os.path.abspath(__file__))
run_py = os.path.join(script_dir, "run-google-java-format.py")
if not os.path.isfile(run_py):
    urllib.urlretrieve("https://raw.githubusercontent.com/mernst/plume-lib/master/bin/run-google-java-format.py", run_py)


temp_dir = tempfile.mkdtemp(prefix='check-google-java-format-')

def temporary_file_name():
    return os.path.join(temp_dir, next(tempfile._get_candidate_names()))

def cleanup():
    shutil.rmtree(temp_dir)


files = sys.argv[1:]
if len(files) == 0:
    content = sys.stdin.read()
    fname = temporary_file_name() + ".java"
    with open(fname,'w') as outfile:
        print(content, file=outfile)
    files = [fname]

temps = []
for fname in files:
    ftemp = temporary_file_name() + ".java"
    shutil.copyfile(fname, ftemp)
    temps.append(ftemp)

# To save one process creation, could call directly in Python.
result = subprocess.call([run_py] + temps)
if result != 0:
    cleanup()
    sys.exit(result)

exit_code = 0

for i in range(len(files)):
    if not filecmp.cmp(files[i], temps[i]):
        # TODO: gives temporary file name if reading from stdin
        print("Improper formatting:", files[i])
        exit_code = 1

cleanup()

sys.exit(exit_code)

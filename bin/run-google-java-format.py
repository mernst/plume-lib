#!/usr/bin/python

# This script reformats each file supplied on the command line according
# to the Google Java style (by calling out to the google-java-format program),
# but with improvements to the formatting of annotations in comments.

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
    urllib.urlretrieve("https://raw.githubusercontent.com/mernst/plume-lib/master/bin/fixup-google-java-format.py", fixup_py)

if debug:
    print("script_dir:", script_dir)
    print("fixup_py: ", fixup_py)
    print("gjf_jar_path: ", gjf_jar_path)

files = sys.argv[1:]
if len(files) == 0:
    print("run-google-java-format.py expects 1 or more filenames as arguments")
    sys.exit(1)

result = subprocess.call(["java", "-jar", gjf_jar_path, "--replace", "--sort-imports=also"] + files)
if result != 0:
    sys.exit(result)
if files == ["--help"]:
    sys.exit(0)
result = subprocess.call([fixup_py] + files)
if result != 0:
    sys.exit(result)

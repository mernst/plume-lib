#!/usr/bin/python

# The google-java-format program reformats Java source code, but it creates
# poor formatting for annotations in comments.
# Run this script on files after running google-java-format, and it will perform
# small changes in place to improve formatting of annotations in comments.
# If called with no arguments, it reads from and writes to standard output.

from __future__ import print_function

import os
import re
import sys

def eprint(*args, **kwargs):
    "Print to standard error"
    print(*args, file=sys.stderr, **kwargs)


# These are annotations defined in the Checker Framework that *should* go
# on their own line.
# (To generate this list, search for occurrences of "@Target("
# and remove those that contain "TYPE_USE" or "{}".)
declarationAnnotations = set([
    "@GetConstructor",
    "@NewInstance",
    "@Invoke",
    "@GetClass",
    "@GetMethod",
    "@ForName",
    "@ReportCreation",
    "@ReportReadWrite",
    "@ReportWrite",
    "@ReportOverride",
    "@ReportInherit",
    "@ReportCall",
    "@ReportUse",
    "@StaticallyExecutable",
    "@InheritedAnnotation",
    "@FromByteCode",
    "@PostconditionAnnotation",
    "@RequiresQualifiers",
    "@DefaultQualifier",
    "@DefaultQualifierInHierarchyInUncheckedCode",
    "@InvisibleQualifier",
    "@TargetLocations",
    "@RequiresQualifier",
    "@ImplicitFor",
    "@StubFiles",
    "@EnsuresQualifiersIf",
    "@EnsuresQualifier",
    "@DefaultInUncheckedCodeFor",
    "@AnnotatedFor",
    "@PreconditionAnnotation",
    "@ConditionalPostconditionAnnotation",
    "@DefaultQualifiers",
    "@FieldIsExpression",
    "@FromStubFile",
    "@Unused",
    "@EnsuresQualifiers",
    "@PolymorphicQualifier",
    "@SubtypeOf",
    "@DefaultQualifierInHierarchy",
    "@DefaultFor",
    "@EnsuresQualifierIf",
    "@MonotonicQualifier",
    "@Pure",
    "@LockingFree",
    "@TerminatesExecution",
    "@Deterministic",
    "@SideEffectFree",
    "@SafeType",
    "@SafeEffect",
    "@UIType",
    "@UIPackage",
    "@PolyUIEffect",
    "@PolyUIType",
    "@UIEffect",
    "@NotOnlyInitialized",
    "@ClassRegexParam",
    "@MethodRegexParam",
    "@MultiMethodRegexParam",
    "@Assignable",
    "@I18nValidFormat",
    "@I18nMakeFormat",
    "@Assignable",
    "@Assignable",
    "@EnsuresLockHeldIf",
    "@HoldingOnEntry",
    "@EnsuresLockHeld",
    "@Holding",
    "@FormatMethod",
    "@ReturnsFormat",
    "@EnsuresNonNull",
    "@Covariant",
    "@EnsuresNonNullIf",
    "@RequiresNonNull",
    "@AssertNonNullIfNonNull",
    "@UsesObjectEquals",
    "@I18nChecksFormat",
    "@MultiClassRegexParam",
    "@MethodTaintingParam",
    "@MultiMethodTaintingParam",
    "@ClassTaintingParam",
    "@MultiClassTaintingParam",
])

debug = False

# Two annotations in a row, or an annotation abutting array brackets "[]".
# Space is inserted between.
abuttingannoRegex = re.compile(r"(/\*@[A-Za-z0-9_]+\*/)(\[\]|/\*@[A-Za-z0-9_]+\*/)")
# Voodoo annotation with extra space after
voodootrailingspaceRegex = re.compile(r"(/\*>>> ?@.*\bthis\*/) (\))")

# An annotation at the end of its line (in capturing group 2).
# Also matches an annotation like /*offset = */ that should appear right
# before the argument it documents.
# The annotation will be moved to the beginning of the following line.
# TODO: should also match non-commented annotations.
# TODO: should search in classpath to determine whether annotation is a type
# annotation, and only move it to the beginning of the following line if so.
trailingannoRegex = re.compile(r"^(.*?)[ \t]*(/\*(@[A-Za-z0-9_().\"]+| *[A-Za-z0-9_]+ *= *)\*/)$")

whitespaceRegex = re.compile(r"^([ \t]*).*$")

emptylineRegex = re.compile(r"^[ \t]*$")

def insert_after_whitespace(insertion, s):
    """Return s, with insertion inserted after its leading whitespace."""
    m = re.match(whitespaceRegex, s)
    return s[0:m.end(1)] + insertion + s[m.end(1):]


def fixup_loop(infile, outfile):
    """Fix up formatting while reading from infile and writing to outfile."""
    prev = ""           # previous line
    for line in infile:
        # Handle trailing space after a voodoo comment
        line = voodootrailingspaceRegex.sub(r"\1\2", line)
        # Handle abutting annotations in comments
        m = re.search(abuttingannoRegex, line)
        while m:
            if debug: print("found abutting", line)
            line = line[0:m.end(1)] + " " + line[m.start(2):]
            m = re.search(abuttingannoRegex, line)
        # Handle annotations at end of line that should be at beginning of
        # next line.
        m = re.search(trailingannoRegex, prev)
        if debug: print("trailing?", m, prev, line)
        while m:
            if debug: print("found trailing", prev, line)
            anno = m.group(2)
            if base_annotation(anno) in declarationAnnotations:
                break
            if debug: print("prev was:", prev)
            prev = prev[0:m.end(1)] + prev[m.end(2):]
            if debug: print("prev is :", prev)
            if re.search(emptylineRegex, prev):
                prev = ""
                if debug: print("prev is':", prev)
            if debug: print("line was:", line)
            line = insert_after_whitespace(m.group(2) + " ", line)
            if debug: print("line is :", line)
            m = re.search(trailingannoRegex, prev)
        outfile.write(prev)
        prev = line
    outfile.write(prev)

def base_annotation(annotation):
    """Remove leading and trailing comment characters, spaces, and arguments.
Example: base_annotation('/*@RequiresNonNull("FileIO.data_trace_state")*/' => '@RequiresNonNull'"""
    if debug: print("base_annotation <=", annotation)
    if annotation.startswith("/*"):
        annotation = annotation[2:]
    if annotation.endswith("*/"):
        annotation = annotation[:-2]
    idx = annotation.find('(')
    if idx != -1:
        annotation = annotation[0:idx]
    annotation = annotation.strip()
    if debug: print("base_annotation =>", annotation)
    return annotation


if len(sys.argv) == 1:
    fixup_loop(sys.stdin, sys.stdout)
else:
    for fname in sys.argv[1:]:
        outfname = fname + '.out'
        with open(fname,'r') as infile:
            with open(outfname ,'w') as outfile:
                fixup_loop(infile, outfile)
        os.rename(outfname, fname)

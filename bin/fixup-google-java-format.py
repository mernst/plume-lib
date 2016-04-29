#!/usr/bin/python

# The google-java-format program reformats Java source code, but it creates
# poor formatting for annotations in comments.
# Run this script on files after running google-java-format, and it will perform
# small changes in place to improve formatting of annotations in comments.
# If called with no arguments, it reads from and writes to standard output.

import os
import re
import sys

# TODO: handle annotations with arguments

# These are annotations defined in the Checker Framework that *should* go
# on their own line.
# (To generate this list, search for occurrences of "@Target("
# and remove those that contain "TYPE_USE" or "{}".)
declarationAnnotations = set([
    "/*@GetConstructor*/",
    "/*@NewInstance*/",
    "/*@Invoke*/",
    "/*@GetClass*/",
    "/*@GetMethod*/",
    "/*@ForName*/",
    "/*@ReportCreation*/",
    "/*@ReportReadWrite*/",
    "/*@ReportWrite*/",
    "/*@ReportOverride*/",
    "/*@ReportInherit*/",
    "/*@ReportCall*/",
    "/*@ReportUse*/",
    "/*@StaticallyExecutable*/",
    "/*@InheritedAnnotation*/",
    "/*@FromByteCode*/",
    "/*@PostconditionAnnotation*/",
    "/*@RequiresQualifiers*/",
    "/*@DefaultQualifier*/",
    "/*@DefaultQualifierInHierarchyInUncheckedCode*/",
    "/*@InvisibleQualifier*/",
    "/*@TargetLocations*/",
    "/*@RequiresQualifier*/",
    "/*@ImplicitFor*/",
    "/*@StubFiles*/",
    "/*@EnsuresQualifiersIf*/",
    "/*@EnsuresQualifier*/",
    "/*@DefaultInUncheckedCodeFor*/",
    "/*@AnnotatedFor*/",
    "/*@PreconditionAnnotation*/",
    "/*@ConditionalPostconditionAnnotation*/",
    "/*@DefaultQualifiers*/",
    "/*@FieldIsExpression*/",
    "/*@FromStubFile*/",
    "/*@Unused*/",
    "/*@EnsuresQualifiers*/",
    "/*@PolymorphicQualifier*/",
    "/*@SubtypeOf*/",
    "/*@DefaultQualifierInHierarchy*/",
    "/*@DefaultFor*/",
    "/*@EnsuresQualifierIf*/",
    "/*@MonotonicQualifier*/",
    "/*@Pure*/",
    "/*@LockingFree*/",
    "/*@TerminatesExecution*/",
    "/*@Deterministic*/",
    "/*@SideEffectFree*/",
    "/*@SafeType*/",
    "/*@SafeEffect*/",
    "/*@UIType*/",
    "/*@UIPackage*/",
    "/*@PolyUIEffect*/",
    "/*@PolyUIType*/",
    "/*@UIEffect*/",
    "/*@NotOnlyInitialized*/",
    "/*@ClassRegexParam*/",
    "/*@MethodRegexParam*/",
    "/*@MultiMethodRegexParam*/",
    "/*@Assignable*/",
    "/*@I18nValidFormat*/",
    "/*@I18nMakeFormat*/",
    "/*@Assignable*/",
    "/*@Assignable*/",
    "/*@EnsuresLockHeldIf*/",
    "/*@HoldingOnEntry*/",
    "/*@EnsuresLockHeld*/",
    "/*@Holding*/",
    "/*@FormatMethod*/",
    "/*@ReturnsFormat*/",
    "/*@EnsuresNonNull*/",
    "/*@Covariant*/",
    "/*@EnsuresNonNullIf*/",
    "/*@RequiresNonNull*/",
    "/*@AssertNonNullIfNonNull*/",
    "/*@UsesObjectEquals*/",
    "/*@I18nChecksFormat*/",
    "/*@MultiClassRegexParam*/",
    "/*@MethodTaintingParam*/",
    "/*@MultiMethodTaintingParam*/",
    "/*@ClassTaintingParam*/",
    "/*@MultiClassTaintingParam*/",
])

debug = False

# Two annotations in a row, or an annotation abutting array brackets "[]".
# Space is inserted between.
abuttingannoRegex = re.compile(r"(/\*@[A-Za-z0-9_]+\*/)(\[\]|/\*@[A-Za-z0-9_]+\*/)")
# Voodoo annotation with extra space after
vodootrailingspaceRegex = re.compile(r"(\(/\*>>> @.*\bthis\*/) (\))")

# An annotation at the end of its line.
# The annotation will be moved to the beginning of the following line.
trailingannoRegex = re.compile(r"^(.*?)[ \t]*(/\*@[A-Za-z0-9_]+\*/)$")
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
        line = vodootrailingspaceRegex.sub(r"\1\2", line)
        m = re.search(abuttingannoRegex, line)
        while m:
            if debug: print "found abutting", line
            line = line[0:m.end(1)] + " " + line[m.start(2):]
            m = re.search(abuttingannoRegex, line)
        m = re.search(trailingannoRegex, prev)
        while m:
            anno = m.group(2)
            if anno in declarationAnnotations:
                break
            if debug: print "prev was:", prev
            prev = prev[0:m.end(1)] + prev[m.end(2):]
            if debug: print "prev is :", prev
            if re.search(emptylineRegex, prev):
                prev = ""
                if debug: print "prev is':", prev
            if debug: print "line was:", line
            line = insert_after_whitespace(m.group(2) + " ", line)
            if debug: print "line is :", line
            m = re.search(trailingannoRegex, prev)
        outfile.write(prev)
        prev = line
    outfile.write(prev)


if len(sys.argv) == 1:
    fixup_loop(sys.stdin, sys.stdout)
else:
    for fname in sys.argv[1:]:
        outfname = fname + '.out'
        with open(fname,'r') as infile:
            with open(outfname ,'w') as outfile:
                fixup_loop(infile, outfile)
        os.rename(outfname, fname)

### User customizations

PLUME_DIR_DEFAULT := $(realpath $(dir $(lastword $(MAKEFILE_LIST)))..)

# Put user-specific changes in your own Makefile.user
#  (such as changes to PLUME_DIR or JAVAC or JAVAC_EXTRA_ARGS).
# Make will silently continue if file does not exist.
-include Makefile.user

PLUME_DIR ?= ${PLUME_DIR_DEFAULT}

UNAME_S := $(shell uname -s)
ifndef JAVA_HOME
  ifeq ($(UNAME_S),Darwin)
    JAVA_HOME := $(shell /usr/libexec/java_home)
  else
    java_cmd := $(shell which java)
    # /bin//java happens when the PATH element ended in a /
    JAVA_HOME := $(subst /bin//java,,$(subst /bin/java,,$(java_cmd)))
  endif
  I_set_JAVA_HOME := true
endif

TOOLS_JAR ?= ${JAVA_HOME}/lib/tools.jar

ifeq ($(UNAME_S),Darwin)
  READLINK_F := readlink
else
  READLINK_F := readlink -f
endif

ifeq "$(wildcard $(TOOLS_JAR))" ""
  ifndef JDK_HOME
    javac_cmd := $(shell $(READLINK_F) `which javac`)
    JDK_HOME := $(subst /bin//javac,,$(subst /bin/javac,,$(javac_cmd)))
    ifeq "$(wildcard $(JDK_HOME)/lib/tools.jar)" ""
      javac_cmd := $(shell $(READLINK_F) /usr/bin/javac)
      JDK_HOME := $(subst /bin//javac,,$(subst /bin/javac,,$(javac_cmd)))
    endif
  endif
  ifneq "$(wildcard $(JDK_HOME)/lib/tools.jar)" ""
    TOOLS_JAR := $(JDK_HOME)/lib/tools.jar
    I_set_TOOLS_JAR := true
  endif
endif


# Cygwin doesn't export OSTYPE.
ifeq (cygwin,$(OSTYPE))
else
  UNAME := $(shell uname)
  ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
    # OSTYPE is a bash variable, not an environment variable.  Set it if necessary
    OSTYPE := cygwin
  endif
endif

# Hack hard-coded for UW CSE file system
ifneq ("$(wildcard /homes/gws/markro/jdk7)","")
  TOOLS_JAR_7 ?= /homes/gws/markro/jdk7/lib/tools.jar
endif
# tools.jar file from JDK 7, so plume.jar is can be run on Java 7 JVM.
TOOLS_JAR_7 ?= ${TOOLS_JAR}
ifeq (cygwin,$(OSTYPE))
  TOOLS_JAR_7 := $(shell cygpath -w $(TOOLS_JAR_7))
endif


# This wildcard doesn't seem to work, so list the jar files individually.
# CLASSPATH:=lib/*:$(JAVA_HOME)/lib/tools.jar
# Don't quote TOOLS_JAR; it will be quoted when it is used
CLASSPATH := .:src:..:lib/bcel.jar:lib/backport-util-concurrent-3.1.jar:lib/checker-qual-2.1.1.jar:lib/commons-codec-1.10.jar:lib/commons-io-2.5.jar:lib/commons-lang-2.6.jar:lib/commons-lang3-3.4.jar:lib/commons-logging-1.2.jar:lib/guava-19.0.jar:lib/hamcrest-core-1.3.jar:lib/ical4j-1.0.6.jar:lib/ini4j-0.5.4.jar:lib/junit-4.12.jar:lib/svnkit-1.8.10-complete.jar:lib/tagsoup-1.2.1.jar:lib/xom-1.2.10.jar:${TOOLS_JAR}

# hack to add a space
nullstring :=
space := $(nullstring) # a space in the end

ifndef HTML_TO_TEXT
ifneq (,$(shell which lynx 2>/dev/null))
  # Note no trailing space!
  HTML_TO_TEXT = lynx -dump file://$(shell pwd)/
else
ifneq (,$(shell which html2text 2>/dev/null))
  # Note trailing space!
  HTML_TO_TEXT = html2text -rcfile html2textrc$(space)
else
  # Note trailing space!
  HTML_TO_TEXT = cat$(space)
endif
endif
endif

# To run doclint, do
#   JAVADOC="~/research/types/jsr308-langtools/dist/bin/javadoc" make javadoc
ifeq (cygwin, $(OSTYPE))
  ## Make the classpath explicit on the command line so it can be processed by *-cygwin
  JAVA ?= ${PLUME_DIR}/bin/java-cygwin -ea -classpath "${CLASSPATH}"
  JAVAC ?= ${PLUME_DIR}/bin/javac-cygwin -classpath "${CLASSPATH}"
  JAVADOC ?= ${PLUME_DIR}/bin/javadoc-cygwin -classpath "${CLASSPATH}"
  JAR ?= ${JAVA_HOME}/bin/jar
else
  JAVA ?= java -ea
  # JAVAC ?= javac -g -Xlint:unchecked
  # JAVAC ?= jikes -g +E +F
  JAVAC ?= javac
  JAVADOC ?= javadoc
  JAR ?= jar
endif

# A user can add flags like "-source 7" or "-target 7" to JAVAC_TARGET_FLAGS.
# That variable appears last and overrides the values we provide here.
JAVAC_ARGS ?= -g -Xlint -Xmaxwarns 1000 -J-Xmx2000m -source 7 -target 7 -Xlint:-options -Werror -Xlint:-classfile ${JAVAC_TARGET_FLAGS}
JAVAC_COMMAND = ${JAVAC} ${JAVAC_ARGS} ${JAVAC_EXTRA_ARGS}

JAVADOC_VERSION := $(shell $(JAVADOC) -version 2>&1)

# Leading "./" is for consistency with output of "find", so that names are
# identical and "sort" can remove duplicates.
AUTO_GENERATED_FILES := \
	src/plume/MathMDE.java
# Call to sort is to remove duplicates, which would cause javac errors.
# Users may override to compile a subset of files.  E.g., src/plume/JWhich.java
JAVA_FILES ?= $(sort $(shell find src -name '*.java' -print | grep -v '\.\#' | sort) ${AUTO_GENERATED_FILES})
JAVA_CPP_FILES := $(shell find src  -name '*.java.jpp' -print | sort)
JAVA_AND_CPP_FILES := ${JAVA_FILES} ${JAVA_CPP_FILES}

# Files whose style we can automatically check
JAVA_FILES_FOR_STYLE ?= $(shell find src -name '*.java' -print | grep -v '\.\#' | grep -v WeakHasherMap.java | grep -v WeakIdentityHashMap.java | grep -v MathMDE.java | sort)

## This is not necessary; the scripts java-cygwin, javac-cygwin, and
## javadoc-cygwin do the path translation.
# # On cygwin, translate the classpath and sourcepath to windows format.
# # Only do so if this has not already been done, which is determined by
# # looking for semicolons (;) in the classpath.
# ifeq (cygwin,$(OSTYPE))
#   ifneq (;,$(findstring ;,$(CLASSPATH)))
#     CLASSPATH := $(shell cygpath -pw "$(CLASSPATH)")
#   endif
#   TOOLS_JAR := '$(shell cygpath -w $(TOOLS_JAR))'
# endif

## Pluggable typechecking

ifdef JSR308
  CHECKERFRAMEWORK ?= ${JSR308}/checker-framework
endif
ifeq "$(wildcard ${CHECKERFRAMEWORK}/checker/dist/checker.jar)" "${CHECKERFRAMEWORK}/checker/dist/checker.jar"
  ifeq "$(wildcard ${CHECKERFRAMEWORK}/checker/build)" "${CHECKERFRAMEWORK}/checker/build"
    CLASSPATH := ${CLASSPATH}:${CHECKERFRAMEWORK}/checker/build
  endif
  CLASSPATH := ${CLASSPATH}:${CHECKERFRAMEWORK}/checker/dist/checker.jar
  JAVAC_COMMAND = ${CHECKERFRAMEWORK}/checker/bin/javac ${JAVAC_ARGS} ${JAVAC_EXTRA_ARGS}
endif

# Note that pluggable type-checking requires checker.jar (not just
# checker-qual.jar) on the classpath.

# -AsuppressWarnings=lock:method.guarantee.violated is temporary. Once the Purity Checker
# and -AsuggestPureMethods are fixed, they can be run on plume-lib and Daikon and this
# can be removed. This warning is issued by the Lock Checker when a method calls another
# method with a weaker side effect guarantee.
JAVACHECK_ARGS ?= -source 7 -target 7 -Xlint:-options -Awarns -implicit:class -Xlint:-processing -AcheckPurityAnnotations -AsuppressWarnings=lock:method.guarantee.violated -processor org.checkerframework.checker.nullness.NullnessRawnessChecker,org.checkerframework.checker.nullness.NullnessChecker,org.checkerframework.checker.interning.InterningChecker,org.checkerframework.checker.regex.RegexChecker,org.checkerframework.checker.formatter.FormatterChecker,org.checkerframework.checker.signature.SignatureChecker,org.checkerframework.checker.lock.LockChecker

## For testing a single checker, replace the above by:
# JAVACHECK_ARGS ?= -source 7 -target 7 -Xlint:-options -Awarns -implicit:class -Xlint:-processing -AcheckPurityAnnotations -processor org.checkerframework.checker.regex.RegexChecker
## For testing a single file:
# JAVA_FILES = src/plume/RegexUtil.java

## For testing invariant arrays, add "-AinvariantArrays" like so:
# JAVACHECK_ARGS ?= -source 7 -target 7 -Xlint:-options -Awarns -implicit:class -Xlint:-processing -AcheckPurityAnnotations -AinvariantArrays -processor org.checkerframework.checker.regex.RegexChecker

## For testing -AsafeDefaultsForUnannotatedBytecode, uncomment:
# JAVACHECK_EXTRA_ARGS ?= -AsafeDefaultsForUnannotatedBytecode -AprintErrorStack -classpath ${CHECKERFRAMEWORK}/checker/lib/java-getopt-1.0.14-annotated.jar:${CLASSPATH}

# JAVACHECK has to be a single file name, because it's checked for existence.
JAVACHECK ?= ${CHECKERFRAMEWORK}/checker/bin/javac
JAVACHECK_COMMAND = ${JAVACHECK} ${JAVAC_ARGS} ${JAVAC_EXTRA_ARGS} ${JAVACHECK_ARGS} ${JAVACHECK_EXTRA_ARGS}

# A user may set this in Makefile.user or on the command line
ifneq ("$(ALWAYS_CHECK_TYPES)","")
  JAVAC_COMMAND=${JAVACHECK_COMMAND}
endif

## ... end pluggable typechecking


export CLASSPATH

default: jar
# No need for optionsdoc here, because javadoc depends on optionsdoc
all: jar javadoc

# Must not depend on javadoc, since javadoc depends on it
compile: compile_without_testing test

compile_without_testing:  .class-files-timestamp

# Don't do this.  We want to create .class-files-timestamp after the
# compilation succeeds.  So, no targets for .class files in this Makefile.
# ${ALL_CLASS_FILES}: .class-files-timestamp

# This rule creates ${ALL_CLASS_FILES}.
# The timestamp file indicates when the files were last compiled.
.class-files-timestamp: ${JAVA_AND_CPP_FILES}
	${MAKE} tools_jar_exists
	${JAVAC_COMMAND} ${JAVA_FILES} 2>&1
	touch .class-files-timestamp

# Remove most generated files, but not jar files which are desired in the release.
clean-except-jar:
	rm -f src/plume/*.class
	rm -f .class-files-timestamp
	rm -f .javadoc-timestamp
	rm -f .optionsdoc-timestamp
	rm -f .test-timestamp

# Remove most generated files.
# The goal is to cause all code to be remade and and tests to be run.
clean: clean-except-jar
	rm -f plume.jar lookup.jar task_manager.jar

very-clean: very_clean

# Remove every generated file.
# The goal is to restore the directory to its original state when checked out.
very_clean: clean
	rm -f ${AUTO_GENERATED_FILES}
	rm -f lookup.html
	rm -f lookup.txt
	rm -f TAGS
	rm -rf api

# Clean in preparation for making a plume-lib release.
# Some generated files are retained for users' convenience.
release_clean: clean-except-jar
	rm -f lookup.html
	rm -f lookup.txt

tags: TAGS
TAGS: ${JAVA_AND_CPP_FILES}
	etags ${JAVA_AND_CPP_FILES}

test: .test-timestamp

# Using compile_without_testing instead of .class-files-timestamp as the
# prerequisite doesn't work.
.test-timestamp: .class-files-timestamp
# CLASSPATH is set in this file; it is not an external dependency
ifdef USE_CODECOV
	set JAVA_COMPILER=NONE; ${JAVA} -javaagent:lib/jacocoagent-0.7.8.jar='excludes=plume.OrderedPairIterator',destfile=../jacoco.exec  -cp ${CLASSPATH}:lib/junit-4.12.jar:lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore plume.TestPlume
else
	set JAVA_COMPILER=NONE; ${JAVA} -cp ${CLASSPATH}:lib/junit-4.12.jar:lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore plume.TestPlume
endif
	touch .test-timestamp

run-chicory:
	set JAVA_COMPILER=NONE; ${JAVA} -ea daikon.Chicory --output-dir=/tmp -- plume.TestPlume --shortrun

run-chicory-arrays:
	set JAVA_COMPILER=NONE; ${JAVA} -ea daikon.Chicory --ppt-select-pattern=ArraysMDE --output-dir=/tmp plume.TestPlume --shortrun


api: javadoc
doc: javadoc
javadoc: .javadoc-timestamp

.javadoc-timestamp: $(JAVA_FILES)
	$(MAKE) optionsdoc
# The "rm -rf" is necessary or else any old version of stylesheet.css is
# retained rather than overwritten
	rm -rf api
	mkdir -p api
	touch ical4j.properties
	${JAVADOC} -d api ${JAVA_FILES}
	rm -f ical4j.properties
	cp -pf ../bin/plume-lib-favicon.png api
	cd api && ../../bin/add-favicon . plume-lib-favicon.png
	# jdk-add-munged-anchor-names api/plume/*.html
	touch .javadoc-timestamp
	@echo "Optionally run javadoc-index-to-alist (for Emacs support)"

# Puts Javadoc at http://types.cs.washington.edu/plume-lib/api/
javadoc-publish: javadoc
	chgrp -R types_www api
	chmod -R o+r api
# This rsync has stopped working, so do the chgrp before the rsync.
	rsync -a api /cse/www2/types/plume-lib/
#	chgrp -R types_www /cse/www2/types/plume-lib/
	chmod -R g+w /cse/www2/types/plume-lib/

optionsdoc: .optionsdoc-timestamp

# This test should use "command -v" instead of "which"
ifndef DOS2UNIX
  WHICHDOS2UNIX := $(shell which dos2unix)
  ifneq ($(WHICHDOS2UNIX),)
    DOS2UNIX = $(WHICHDOS2UNIX) --quiet
  else
    ifneq ($(shell which perl),)
      DOS2UNIX = perl -pi -e 's/\r\n|\n|\r/\n/g'
    else
      $(error Install either dos2unix or perl)
    endif
  endif
endif

# This target does not create any new files -- it just updates existing ones.
.optionsdoc-timestamp: $(JAVA_FILES)
# This should presumably test for other buggy versions of gjdoc as well.
# The symptom is output such as:  ARGH! @Option
ifeq (gjdoc 0.7.9,$(JAVADOC_VERSION))
	echo "Skipping optionsdoc target because of bugs in $(JAVADOC) version $(JAVADOC_VERSION)"
else
#	@echo "JAVADOC = $(JAVADOC)"
#	@echo "JAVADOC_VERSION = $(JAVADOC_VERSION)"
#Can also use -verbose for debugging
	$(MAKE) compile
	$(JAVADOC) -quiet -docletpath "${CLASSPATH}" -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/Lookup.java src/plume/Lookup.java
	$(DOS2UNIX) src/plume/Lookup.java
	$(MAKE) lookup.html lookup.txt
	@touch ical4j.properties
	$(JAVADOC) -quiet -docletpath "${CLASSPATH}" -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/ICalAvailable.java src/plume/ICalAvailable.java
	$(DOS2UNIX) src/plume/ICalAvailable.java
	@rm -f ical4j.properties
	$(JAVADOC) -quiet -docletpath "${CLASSPATH}" -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/MultiVersionControl.java src/plume/MultiVersionControl.java
	$(DOS2UNIX) src/plume/MultiVersionControl.java
	$(JAVADOC) -quiet -docletpath "${CLASSPATH}" -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/TaskManager.java src/plume/TaskManager.java
	$(DOS2UNIX) src/plume/TaskManager.java
	touch .optionsdoc-timestamp
endif

tools_jar_exists:
ifeq (cygwin,$(OSTYPE))
# Give up on verifying existence of tools.jar (which contains a space in the file name) on Cygwin
	@true
else
	$(MAKE) ${TOOLS_JAR}
endif

# This target only fires if TOOLS_JAR does not exist
# The make program has serious problems with filenames containing spaces, unfortunately.
${TOOLS_JAR}:
	@echo "Cannot find tools.jar at ${TOOLS_JAR}"
	@echo "Please set JAVA_HOME and/or TOOLS_JAR"
# Cause the Make job to fail
	@false


showvars:
# Short output
	@echo OSTYPE = $(OSTYPE)
	@echo JAVA_HOME = "$(JAVA_HOME)"
	@echo javac_cmd = $(javac_cmd)
	@echo JAVAC_COMMAND = $(JAVAC_COMMAND)
	@echo JAVACHECK_COMMAND = $(JAVACHECK_COMMAND)
	@echo JDK_HOME = $(JDK_HOME)
	@echo TOOLS_JAR = $(TOOLS_JAR)
	@echo I_set_TOOLS_JAR = $(I_set_TOOLS_JAR)
# Long output
	@echo JAVA_AND_CPP_FILES = $(JAVA_AND_CPP_FILES)
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo JAVA_CPP_FILES = $(JAVA_CPP_FILES)
	@echo CLASSPATH = "$(CLASSPATH)"


###########################################################################
### Automatically generated files
###

src/plume/MathMDE.java: src/plume/MathMDE.java.jpp src/plume/MathMDE-gcd.java.jpp src/plume/MathMDE-modulus.java.jpp src/plume/MathMDE-nonmodulus.java.jpp src/plume/MathMDE-help.java.jpp src/plume/MathMDE-helpend.java.jpp
	@rm -f $@
	(cd src/plume && ../../../bin/java-cpp MathMDE.java.jpp 2>&1 > MathMDE.java)
	@chmod -w $@


###########################################################################
### Jar files and distribution
###

## TODO:  the .jar files other than plume.jar should be trimmed, with
## extraneous components removed.

# TODO: add the checkenv target as dependency for more targets?
checkenv:
ifeq "$(I_set_JAVA_HOME)" "true"
	@echo "WARNING: JAVA_HOME is not set, assuming $(JAVA_HOME)"
endif

jar: checkenv plume.jar lookup.jar task_manager.jar

# This should depend on all the included .jar files, too.
plume.jar: ${JAVA_AND_CPP_FILES} lib
# "make clean" avoids including stray .class files from your directory.
# It does cause some extra work, but so be it.
# "make compile" runs unit tests; do that instead of "compile_without_testing".
# If you use "compile_without_testing", the tests will look out of date
# with respect to the newly-generated .class files.
# But, don't make "clean compile" the prerequisites for plume.jar or the
# .jar file will always be remade even if it is up-to-date.
	$(MAKE) tools_jar_exists
	$(MAKE) clean optionsdoc
	$(MAKE) compile
	-rm -rf jar-contents
	mkdir jar-contents
	mkdir jar-contents/plume
	cp -p src/plume/*.class jar-contents/plume
	(cd jar-contents; "${JAR}" xf ../lib/backport-util-concurrent-3.1.jar)
	(cd jar-contents; "${JAR}" xf ../lib/bcel.jar)
	(cd jar-contents; "${JAR}" xf ../lib/checker-qual-2.1.1.jar)
	(cd jar-contents; "${JAR}" xf ../lib/commons-codec-1.10.jar)
	(cd jar-contents; "${JAR}" xf ../lib/commons-io-2.5.jar)
# ical4j 1.0.6 requrires the legacy library commons-lang-2.6.jar
	(cd jar-contents; "${JAR}" xf ../lib/commons-lang-2.6.jar)
	(cd jar-contents; "${JAR}" xf ../lib/commons-lang3-3.4.jar)
	(cd jar-contents; "${JAR}" xf ../lib/commons-logging-1.2.jar)
	(cd jar-contents; "${JAR}" xf ../lib/guava-19.0.jar)
	(cd jar-contents; "${JAR}" xf ../lib/ical4j-1.0.6.jar)
	(cd jar-contents; "${JAR}" xf ../lib/ini4j-0.5.4.jar)
# Do not include junit.jar in plume.jar; JUnit is needed only for testing
#	(cd jar-contents; "${JAR}" xf ../lib/hamcrest-core-1.3.jar)
#	(cd jar-contents; "${JAR}" xf ../lib/junit-4.12.jar)
	(cd jar-contents; "${JAR}" xf ../lib/svnkit-1.8.10-complete.jar)
	(cd jar-contents; "${JAR}" xf ../lib/tagsoup-1.2.1.jar)
	(cd jar-contents; "${JAR}" xf ../lib/xom-1.2.10.jar)
	(cd jar-contents; "${JAR}" xf "$(TOOLS_JAR_7)" com/sun/javadoc)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
# Put contents in alphabetical order.
	(cd jar-contents; find * -type f | ${PLUME_DIR}/bin/sort-directory-order > jar-contents.txt)
# Temporarily commented out so that plume-lib can be built with Java 8;
# but I need to find a good way to double-check most builds.
	(cd jar-contents; "${JAR}" cf ../plume.jar @jar-contents.txt)
	rm -rf jar-contents

# Fails if plume.jar has Java 8 .class files, which cannot be used on Java 7 JVM
verify-plume-jar-classfile-version:
	rm -rf jar-contents
	mkdir jar-contents
	(cd jar-contents; jar xf ../plume.jar)
	(cd jar-contents; find . -type f -name '*.class' | xargs file | grep 'version 52\.0' > version-52-files.txt || true)
	if [ -s jar-contents/version-52-files.txt ] ; then echo "Version 52.0 files exist: see list in jar-contents/version-52-files.txt"; exit 1; fi
	rm -rf jar-contents


###
### Lookup targets
###

lookup.html: src/plume/Lookup.java
	${JAVADOC} -docletpath "${CLASSPATH}" -J-ea -quiet -doclet plume.OptionsDoclet -classdoc $< > $@
lookup.txt: lookup.html
# Note no space between command and argument.
	$(HTML_TO_TEXT)$< > $@

LOOKUP_JAR_CLASSES = \
	src/plume/Lookup.class \
	src/plume/RegexUtil.class \
	src/plume/UtilMDE*.class src/plume/EntryReader*.class \
	src/plume/Option*.class src/plume/StringBuilderDelimited.class \
	src/plume/Unpublicized.class src/plume/SimpleLog.class

# So that lookup.jar is self-contained for execution on JDK 6 (without JDK
# 7 annotation support), the files should have been compiled with a Java 6
# compiler.  Here are two ways to do it.  $JDK6 might be at
# /usr/lib/jvm/java-6-openjdk or /usr/java/current, for example.
# Example 1:
#	make JAVA_HOME=$JDK6 JAVAC=$JDK6/bin/javac clean plume.jar lookup.jar
# Example 2:
#	make TOOLS_JAR=$JDK6/lib/tools.jar clean plume.jar lookup.jar
lookup.jar : .class-files-timestamp lookup.manifest lookup.txt lookup.html
	echo "Make lookup.jar"
	-rm -rf jar-contents
	mkdir jar-contents
	mkdir jar-contents/plume
	cp -p $(LOOKUP_JAR_CLASSES) jar-contents/plume
	cp -p lookup.txt lookup.html jar-contents/plume
	(cd jar-contents; find * -type f | ${PLUME_DIR}/bin/sort-directory-order > jar-contents.txt)
	(cd jar-contents; "${JAR}" cfm ../lookup.jar ../lookup.manifest @jar-contents.txt)
	rm -rf jar-contents
	echo "Done making lookup.jar"

install-lookup: $(pag)/software/pkg/lookup.jar
$(pag)/software/pkg/lookup.jar: lookup.jar
	cp -p $< $>

###
### TaskManager targets
###

TASKMANAGER_JAVA_FILES := $(shell find . -name '*.java')
task-manager : task_manager.jar
task_manager.jar: plume.jar task_manager.manifest
	-rm -rf jar-contents
	mkdir jar-contents
	(cd jar-contents; "${JAR}" xf ../plume.jar)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
	(cd jar-contents; find * -type f | ${PLUME_DIR}/bin/sort-directory-order > jar-contents-list.txt)
	(cd jar-contents; "${JAR}" cfm ../task_manager.jar ../task_manager.manifest @jar-contents-list.txt)
	rm -rf jar-contents

install-task-manager: $(pag)/software/pkg/task_manager.jar
$(pag)/software/pkg/task_manager.jar: task_manager.jar
	cp -p $< $>


# No need to do "make doc" or "make all" before running this.
# (This includes .class files, so users don't need to recompile.)
plume.tar.gz: $(JAVA_AND_CPP_FILES) Makefile README
	rm -rf /tmp/plume
	mkdir /tmp/plume
	cp -p $(JAVA_AND_CPP_FILES) /tmp/plume
	cp -p Makefile README /tmp/plume
	cp -r lib /tmp/plume
	(cd /tmp/plume; make compile_without_testing doc)
	(cd /tmp; tar zcf plume.tar.gz plume)
	rm -f plume.tar.gz
	mv /tmp/plume.tar.gz .
	rm -rf /tmp/plume



###########################################################################
### Code quality
###

# This compiles the code as well as does pluggable type-checking.
typecheck checkers check-types: ${JAVA_AND_CPP_FILES}
	\rm -f .class-files-timestamp
# Possible quoting problem if JAVACHECK_COMMAND contains double-quote characters.
	${MAKE} JAVAC_COMMAND="${JAVACHECK_COMMAND}"

# I have not found this tool very useful.
checkstyle: ${JAVA_AND_CPP_FILES}
# This invocation works on Ubuntu.
	checkstyle -c /usr/share/checkstyle/sun_checks.xml ${JAVA_FILES_FOR_STYLE}

get-run-google-java-format:
	@-git -C .run-google-java-format pull -q || git clone -q https://github.com/plume-lib/run-google-java-format.git .run-google-java-format

# Requires Java 8
reformat:
	${MAKE} get-run-google-java-format
	@.run-google-java-format/run-google-java-format.py ${JAVA_FILES_FOR_STYLE}

# Requires Java 8
check-format:
	${MAKE} get-run-google-java-format
	@.run-google-java-format/check-google-java-format.py ${JAVA_FILES_FOR_STYLE}

.PHONY: typecheck checkers check-types checkstyle reformat check-format


###########################################################################
### Makefile bookkeeping
###

.PHONY: default all compile_without_testing jar clean very_clean release_clean tags test run-chicory javadoc showvars doc api task-manager install-task-manager install-lookup optionsdoc checkenv tools_jar_exists


# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:

# end

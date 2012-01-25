### User customizations

# Put user-specific changes in your own Makefile.user
#  (such as changes to PLUME_DIR or JAVAC or JAVAC_EXTRA_ARGS).
# Make will silently continue if file does not exist.
-include Makefile.user

ifndef JAVA_HOME
  java_cmd := $(shell which java)
  # /bin//java happens when the PATH element ended in a /
  JAVA_HOME := $(subst /bin//java,,$(subst /bin/java,,$(java_cmd)))
  I_set_JAVA_HOME := true
endif


# On MacOS, there is no separate tools.jar
ifneq (,$(findstring darwin,$(OSTYPE)))
  TOOLS_JAR ?= /System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Classes/classes.jar
else
  TOOLS_JAR ?= ${JAVA_HOME}/lib/tools.jar
endif

# tools.jar file from JDK 5, so plume.jar is fully compatible with Java version 5.
TOOLS_JAR_5 ?= ${TOOLS_JAR}

# This wildcard doesn't seem to work, so list the jar files individually.
# CLASSPATH:=lib/*:$(JAVA_HOME)/lib/tools.jar
CLASSPATH := .:src:..:lib/bcel.jar:lib/checkers-quals.jar:lib/commons-logging-1.1.1.jar:lib/commons-io-1.4.jar:lib/commons-lang-2.4.jar:lib/ical4j.jar:lib/ini4j-0.5.1.jar:lib/junit-4.8.2.jar:lib/svnkit.jar:lib/tagsoup-1.2.jar:lib/xom-1.2.1.jar:${TOOLS_JAR}

# On cygwin, translate the classpath and sourcepath to windows format.
# Only do so if this has not already been done, which is determined by
# looking for semicolons (;) in the classpath.
ifeq (cygwin,$(OSTYPE))
  ifneq (;,$(findstring ;,$(CLASSPATH)))
    CLASSPATH := $(shell cygpath -pw "$(CLASSPATH)")
  endif
  TOOLS_JAR := '$(shell cygpath -w $(TOOLS_JAR))'
endif

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

JAVAC ?= javac
# Add flags like "-source 5" or "-target 5" to JAVAC_TARGET_FLAGS.
JAVAC_ARGS ?= -g -Xlint -Xmaxwarns 1000 -J-Xmx800m ${JAVAC_TARGET_FLAGS}
# When using the JSR 308 compiler, this expects checkers-quals.jar on the classpath.
# The "-classpath" argument is only necessary when using the JSR 308 javac,
# since it's a shell command for "javac -jar ..." which ignores the
# CLASSPATH environment variable and thus must be supplied explicitly.
JAVAC_COMMAND = ${JAVAC} ${JAVAC_ARGS} ${JAVAC_EXTRA_ARGS}
# JAVAC ?= javac -g -Xlint:unchecked
# JAVAC ?= jikes -g +E +F
JAVA ?= java -ea
JAVADOC ?= javadoc

JAR ?= jar

JAVADOC_VERSION := $(shell $(JAVADOC) -version 2>&1)

# Leading "./" is for consistency with output of "find", so that names are
# identical and "sort" can remove duplicates.
AUTO_GENERATED_FILES := \
	src/plume/MathMDE.java
# Call to sort is to remove duplicates, which would cause javac errors.
# Users may override to compile a subset of files.  E.g., src/plume/JWhich.java
JAVA_FILES ?= $(sort $(shell find src \( -name CVS \) -prune -o -name '*.java' -print | grep -v '\.\#' | sort) ${AUTO_GENERATED_FILES})
JAVA_CPP_FILES := $(shell find src \( -name CVS \) -prune -o -name '*.java.jpp' -print | sort)
JAVA_AND_CPP_FILES := ${JAVA_FILES} ${JAVA_CPP_FILES}

ifdef JSR308
  CHECKERS ?= ${JSR308}/checker-framework/checkers
endif
ifdef CHECKERS
  CLASSPATH := ${CHECKERS}/checkers.jar:${CLASSPATH}
  ifeq "$(wildcard ${CHECKERS}/build)" "${CHECKERS}/build"
    CLASSPATH := ${CHECKERS}/build:${CLASSPATH}
  endif
endif
jsr308_imports=checkers.interning.quals.*:checkers.nullness.quals.*:checkers.regex.quals.*:checkers.signature.quals.*

export jsr308_imports
export CLASSPATH

default: jar
all: jar javadoc

# Must not depend on javadoc, since javadoc depends on it
compile: compile_without_testing test

compile_without_testing:  .class-files-timestamp

# Don't do this.  We want to create .class-files-timestamp after the
# compilation succeeds.  So, no targets for .class files in this Makefile.
# ${ALL_CLASS_FILES}: .class-files-timestamp

# This rule creates ${ALL_CLASS_FILES}.
# The timestamp file indicates when the files were last compiled.
.class-files-timestamp: ${JAVA_AND_CPP_FILES} ${TOOLS_JAR}
	${JAVAC_COMMAND} ${JAVAC_EXTRA_ARGS} ${JAVA_FILES} 2>&1
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

# This "set" form doesn't seem to work for me (maybe needs to be export?).
# Used to depend on targets "clean all", but I want to run this in "all".
test: .test-timestamp

# Using compile_without_testing instead of .class-files-timestamp as the
# prerequisite doesn't work.
.test-timestamp: .class-files-timestamp
# CLASSPATH is set in this file; it is not an external dependency
	set JAVA_COMPILER=NONE; ${JAVA} plume.TestPlume
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
	\rm -f ical4j.properties
	# jdk-add-munged-anchor-names api/plume/*.html
	touch .javadoc-timestamp
	@echo "Optionally run javadoc-index-to-alist (for Emacs support)"

# Puts Javadoc at http://types.cs.washington.edu/plume-lib/api/
javadoc-publish: javadoc
	rsync -a api /cse/www2/types/plume-lib/
	chgrp -R types_www /cse/www2/types/plume-lib/
	chmod -R g+w /cse/www2/types/plume-lib/

optionsdoc: .optionsdoc-timestamp

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
	$(JAVADOC) -quiet -docletpath ${CLASSPATH} -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/Lookup.java src/plume/Lookup.java
	$(MAKE) lookup.html lookup.txt
	@touch ical4j.properties
	$(JAVADOC) -quiet -docletpath ${CLASSPATH} -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/ICalAvailable.java src/plume/ICalAvailable.java
	@\rm -f ical4j.properties
	$(JAVADOC) -quiet -docletpath ${CLASSPATH} -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/MultiVersionControl.java src/plume/MultiVersionControl.java
	$(JAVADOC) -quiet -docletpath ${CLASSPATH} -doclet plume.OptionsDoclet -format javadoc -i -docfile src/plume/TaskManager.java src/plume/TaskManager.java
	touch .optionsdoc-timestamp
endif

${TOOLS_JAR}:
ifneq (,$(findstring darwin,$(OSTYPE)))
	@echo "Cannot find classes.jar at ${TOOLS_JAR}"
	@echo "Please set TOOLS_JAR [sic] to its location"
else
	@echo "Cannot find tools.jar at ${TOOLS_JAR}"
	@echo "Please set JAVA_HOME and/or TOOLS_JAR"
endif
# Cause the Make job to fail
	@false


showvars:
	@echo JAVA_AND_CPP_FILES = $(JAVA_AND_CPP_FILES)
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo JAVA_CPP_FILES = $(JAVA_CPP_FILES)
	@echo JAVA_HOME = "$(JAVA_HOME)"
	@echo JAVAC_COMMAND = $(JAVAC_COMMAND)
	@echo CLASSPATH = "$(CLASSPATH)"
	@echo jsr308_imports = $(jsr308_imports)


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
plume.jar: ${JAVA_AND_CPP_FILES} lib ${TOOLS_JAR}
# "make clean" avoids including stray .class files from your directory.
# "make compile" runs unit tests; do that instead of "compile_without_testing".
# If you use "compile_without_testing", the tests will look out of date
# with respect to the newly-generated .class files.
# But, don't make "clean compile" the prerequisites for plume.jar or the
# .jar file will always be remade even if it is up-to-date.
	$(MAKE) clean optionsdoc
	$(MAKE) compile
	-rm -rf jar-contents
	mkdir jar-contents
	mkdir jar-contents/plume
	cp -p src/plume/*.class jar-contents/plume
	(cd jar-contents; jar xf ../lib/bcel.jar)
	(cd jar-contents; jar xf ../lib/commons-logging-1.1.1.jar)
	(cd jar-contents; jar xf ../lib/commons-io-1.4.jar)
	(cd jar-contents; jar xf ../lib/commons-lang-2.4.jar)
	(cd jar-contents; jar xf ../lib/checkers-quals.jar)
	(cd jar-contents; jar xf ../lib/ical4j.jar)
	(cd jar-contents; jar xf ../lib/ini4j-0.5.1.jar)
	(cd jar-contents; jar xf ../lib/junit-4.8.2.jar)
	(cd jar-contents; jar xf ../lib/svnkit.jar)
	(cd jar-contents; jar xf ../lib/tagsoup-1.2.jar)
	(cd jar-contents; jar xf ../lib/xom-1.2.1.jar)
	(cd jar-contents; jar xf $(TOOLS_JAR_5) com/sun/javadoc)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
# Put contents in alphabetical order. (not for now, overflows command line)
#	(cd jar-contents; jar cf ../plume.jar `find * -type f | sort`)
	cd jar-contents; jar cf ../plume.jar *
	rm -rf jar-contents

###
### Lookup targets
###

lookup.html: src/plume/Lookup.java
	${JAVADOC} -docletpath ${CLASSPATH} -J-ea -quiet -doclet plume.OptionsDoclet -classdoc $< > $@
lookup.txt: lookup.html
# Note no space between command and argument.
	$(HTML_TO_TEXT)$< > $@

LOOKUP_JAR_CLASSES = \
	src/plume/Lookup.class \
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
	-rm -rf jar-contents
	mkdir jar-contents
	mkdir jar-contents/plume
	cp -p $(LOOKUP_JAR_CLASSES) jar-contents/plume
	cp -p lookup.txt lookup.html jar-contents/plume
	cd jar-contents; jar cfm ../lookup.jar ../lookup.manifest *
	rm -rf jar-contents

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
	(cd jar-contents; jar xf ../plume.jar)
	rm -rf jar-contents/meta-inf jar-contents/META-INF
	(cd jar-contents; $(JAR) cfm ../task_manager.jar ../task_manager.manifest `find * -type f | sort`)
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
### Pluggable type-checking
###

## These targets require checkers.jar (not just checkers-quals.jar) on the classpath.

CHECKERS_BOOTCLASSPATH = -Xbootclasspath/p:${CHECKERS}/jdk/jdk.jar
CHECKERS_ARGS = -source 8 -Awarns

JAVAC_NULLNESS_ARGS = -processor checkers.nullness.NullnessChecker -implicit:class -Xlint:-processing
# A user may set JAVAC_USER_NULLNESS_ARGS to override or augment the above.

check-nullness: checkenv
	${JAVAC_COMMAND} ${JAVAC_NULLNESS_ARGS} ${JAVAC_USER_NULLNESS_ARGS} ${CHECKERS_BOOTCLASSPATH} ${CHECKERS_ARGS} ${JAVA_FILES}

JAVAC_INTERNING_ARGS = -processor checkers.interning.InterningChecker -implicit:class -Xlint:-processing
# A user may set JAVAC_USER_INTERNING_ARGS to override or augment the above.

# Why doesn't this need the CHECKERS_BOOTCLASSPATH argument?  It seems
# fine without it...
check-interning: checkenv
	${JAVAC_COMMAND} ${JAVAC_INTERNING_ARGS} ${JAVAC_USER_INTERNING_ARGS} ${CHECKERS_ARGS} ${JAVA_FILES}

JAVAC_REGEX_ARGS = -processor checkers.regex.RegexChecker -implicit:class -Xlint:-processing
# A user may set JAVAC_USER_REGEX_ARGS to override or augment the above.

# Why doesn't this need the CHECKERS_BOOTCLASSPATH argument?  It seems
# fine without it...
check-regex: checkenv
	${JAVAC_COMMAND} ${JAVAC_REGEX_ARGS} ${JAVAC_USER_REGEX_ARGS} ${CHECKERS_ARGS} ${JAVA_FILES}

JAVAC_SIGNATURE_ARGS = -processor checkers.signature.SignatureChecker -implicit:class -Xlint:-processing
# A user may set JAVAC_USER_SIGNATURE_ARGS to override or augment the above.

# Why doesn't this need the CHECKERS_BOOTCLASSPATH argument?  It seems
# fine without it...
check-signature: checkenv
	${JAVAC_COMMAND} ${JAVAC_SIGNATURE_ARGS} ${JAVAC_USER_SIGNATURE_ARGS} ${CHECKERS_ARGS} ${JAVA_FILES}



###########################################################################
### Makefile bookkeeping
###

.PHONY: default all compile_without_testing jar clean very_clean release_clean tags test run-chicory javadoc showvars doc api task-manager install-task-manager install-lookup optionsdoc checkenv


# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:

# end

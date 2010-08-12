.PHONY: java jar
java:
	${MAKE} -C java jar javadoc
jar:
	${MAKE} -C java jar

.PHONY: emacs
emacs:
	${MAKE} -C emacs

# This is not the default target, because it isn't strictly necessary to
# compile the .el files and because errors can arise.  For example, to
# compile or use bbdb-mew requires that the non-standard mew package is
# installed.
all: java emacs

# Default target
all-but-emacs: bin java git-hooks

# This is not the default target, because it isn't strictly necessary to
# compile the .el files and because errors can arise.  For example, to
# compile or use bbdb-mew requires that the non-standard mew package is
# installed.
all: all-but-emacs emacs

# Run tests
.PHONY: bin
bin:
	${MAKE} -C bin

# Compile Java files
.PHONY: java jar
java:
	${MAKE} -C java all
jar:
	${MAKE} -C java jar

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: bin/plume-lib.pre-commit
	cp -pf $< $@
.git/hooks/post-merge: bin/plume-lib.post-merge
	cp -pf $< $@

# Compile Emacs Lisp files
.PHONY: emacs
emacs:
	${MAKE} -C emacs

# Remove files that should not appear in the release.
# Don't run this unless making a release!  And don't run it in your main clone!
# It removes files that appear in the version control system.
.PHONY: release-clean
release-clean:
	rm -rf .git .gitignore
	${MAKE} -C java release_clean

.PHONY: clean
clean:
	${MAKE} -C java clean

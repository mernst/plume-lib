test: prune-xlint-test-1-actual prune-xlint-test-2-actual prune-xlint-test-3-actual prune-xlint-test-4-actual
	diff prune-xlint-test-1-goal prune-xlint-test-1-actual
	diff prune-xlint-test-2-goal prune-xlint-test-2-actual
	diff prune-xlint-test-3-goal prune-xlint-test-3-actual
	diff prune-xlint-test-4-goal prune-xlint-test-4-actual

prune-xlint-test-1-actual: ../../bin/javac-xlint-prune
	-../../bin/javac-xlint-prune prune-xlint-test-1 > $@

prune-xlint-test-2-actual: ../../bin/javac-xlint-prune
	-../../bin/javac-xlint-prune prune-xlint-test-2 > $@

prune-xlint-test-3-actual: ../../bin/javac-xlint-prune
	-../../bin/javac-xlint-prune prune-xlint-test-3 > $@

prune-xlint-test-4-actual: ../../bin/javac-xlint-prune
	-../../bin/javac-xlint-prune prune-xlint-test-4 > $@

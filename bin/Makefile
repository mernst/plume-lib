PYTHON_PROGRAMS = `grep -l '/usr/bin/env python' * | grep -v '~' | grep -v Makefile`

test:
	${MAKE} -C checklink-persistent-errors-test test
	${MAKE} -C sort-directory-order-test test

check-python:
	@pylint --disable=line-too-long,superfluous-parens,invalid-name,too-many-branches,multiple-statements --output-format=parseable --reports=no ${PYTHON_PROGRAMS}

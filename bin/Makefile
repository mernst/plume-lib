PYTHON_PROGRAMS = `grep -l '/usr/bin/env python' * | grep -v '~' | grep -v Makefile`

test:
	${MAKE} -C checklink-persistent-errors-test test
	${MAKE} -C sort-directory-order-test test
	${MAKE} -C lines-test test

check-python:
	@pylint --disable=invalid-name,line-too-long,locally-disabled,multiple-statements,superfluous-parens,too-many-branches --output-format=parseable --reports=no ${PYTHON_PROGRAMS}

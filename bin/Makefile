PYTHON_PROGRAMS = `grep -l '/usr/bin/env python' * | grep -v '~' | grep -v Makefile`

test:
	${MAKE} -C ../tests

check-python:
	@pylint --disable=invalid-name,line-too-long,locally-disabled,multiple-statements,superfluous-parens,too-many-branches --output-format=parseable --reports=no ${PYTHON_PROGRAMS}

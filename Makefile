all:
	${MAKE} -C emacs
	${MAKE} -C java jar javadoc

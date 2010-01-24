all:
	cd emacs && emacs -batch -l ${HOME}/.emacs -f batch-byte-compile *.el
	${MAKE} -C java jar javadoc

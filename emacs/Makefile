EL_FILES = $(wildcard *.el)
ELC_FILES = $(patsubst %.el,%.elc,$(EL_FILES))

.PHONY: compile
compile: ${ELC_FILES}

%.elc: %.el
	emacs -batch -l ${HOME}/.emacs -f batch-byte-compile $<

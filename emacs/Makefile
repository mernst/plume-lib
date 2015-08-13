EL_FILES = $(wildcard *.el)
ELC_FILES = $(patsubst %.el,%.elc,$(EL_FILES))

.PHONY: compile
compile: ${ELC_FILES}

%.elc: %.el
	emacs -q --batch --eval "(add-to-list 'load-path \".\")" -f batch-byte-compile $<

clean:
	rm -f *.elc

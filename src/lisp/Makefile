.PHONY: clean all

lisp-files = $(wildcard *.lisp)
fasls = $(wildcard *.fasl)

systems := mizar
asdfs = $(addsuffix .asd,$(systems))
editable-files := $(lisp-files) $(asdfs) Makefile
emacs-backups := $(wildcard *~) $(wildcard .*~)

clean:
	rm -f $(emacs-backups) $(fasls)

all:
	# nothing yet

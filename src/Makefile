.PHONY: all

subdirs = perl lisp xslt

editable-files = Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

all:
	for dir in $(subdirs); do make -C $$dir all; done

check:
	for dir in $(subdirs); do make -C $$dir check; done

clean:
	for dir in $(subdirs); do make -C $$dir clean; done
	rm -f $(emacs-backups)

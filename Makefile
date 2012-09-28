.PHONY: all

subdirs = bin lisp perl xsl

all:
	for dir in $(subdirs); do make -C $$dir; done

check:
	for dir in $(subdirs); do make -C $$dir check; done

clean:
	for dir in $(subdirs); do make -C $$dir clean; done

.PHONY: all clean

subdirs := bin lib

all:
	for dir in $(subdirs); do make -C $$dir all; done

clean:
	for dir in $(subdirs); do make -C $$dir clean; done

check:
	for dir in $(subdirs); do make -C $$dir check; done

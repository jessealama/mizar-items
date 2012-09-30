.PHONY: clean all

modules := Article         \
           Item            \
           ItemizedArticle \
           LocalDatabase   \
           Mizar           \
           Utils           \
           Xsltproc

pms := $(addsuffix .pm,$(modules))
editable-files := $(pms) Makefile
emacs-backups := $(addsuffix ~,$(editable-files))

clean:
	rm -f $(emacs-backups)

all:
	# nothing yet

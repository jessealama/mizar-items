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

check:
	ok=1; \
	for module in $(pms); do perl -c $$module || ok=0; done; \
	test "$$ok" = "1";

.PHONY: clean all

lisp-files = 100theorems \
             article \
             author \
             bidirectional \
             config \
             depgraph \
             file-utils \
             fragment \
             item \
             itemize \
             itemized-db \
             ivy-nd \
             landmarks \
             library \
             local-db \
             minimize \
             mizar-condition \
             mizar-xsl \
             mizar \
             mml \
             mptp \
             msc \
             packages \
             piotr \
             properties \
             queue \
             run \
             sandbox \
             search \
             server \
             site-about \
             site-data \
             site-paths \
             site-search \
             site-test \
             site \
             stylesheets \
             tc \
             utils \
             xml-utils \

lisps := $(addsuffix .lisp,$(lisp-files))
systems := mizar
asdfs = $(addsuffix .asd,$(systems))
editable-files := $(lisps) $(asdfs) Makefile
emacs-backups := $(addsuffix ~,$(editable-files))

clean:
	rm -f $(emacs-backups)

all:
	# nothing yet

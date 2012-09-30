.PHONY: all

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
             xml-utils
asdfs = mizar
lisps = $(addsuffix .lisp,$(lisp-files))
asds = $(addsuffix .asd,$(asdfs))
editable-files = $(lisps) $(asds) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))
ccl-fasls = $(addsuffix .dx64fsl,$(lisp-files))
fasls = $(ccl-fasls)

all:
	# nothing to do -- yet

clean:
	rm -f $(emacs-backups)
	rm -f $(fasls)

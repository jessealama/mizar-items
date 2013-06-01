pp-srcdir = /Users/alama/sources/mizar/parsing
mizar-items-srcdir = /Users/alama/sources/mizar/mizar-items
mizar-items-xsldir = $(mizar-items-srcdir)/src/xslt
mizar-items-makedir = $(mizar-items-srcdir)/src/make
mizar-items-bindir = $(mizar-items-srcdir)/bin

pp-stylesheet = $(pp-srcdir)/pp.xsl
wrm-stylesheet = $(mizar-items-xsldir)/wrm.xsl
expand-canceled-stylesheet = $(mizar-items-xsldir)/expand-canceled.xsl
split-stylesheet = $(mizar-items-xsldir)/split.xsl
factor-proofs-stylesheet = $(mizar-items-xsldir)/factor-proofs.xsl
check-factorization-stylesheet = $(mizar-items-xsldir)/check-factorization.xsl
itemize-stylesheet = $(mizar-items-xsldir)/itemize.xsl
itemize-makefile-stylesheet = $(mizar-items-xsldir)/itemize-makefile.xsl

# makefiles
normalize-makefile = $(mizar-items-makedir)/normalize.makefile
itemize-makefile = $(mizar-items-makedir)/itemize.makefile

# scripts
itemized-article-dependencies = $(mizar-items-bindir)/itemized-article-dependencies.pl

.SECONDARY:

pp-srcdir = /Users/alama/sources/mizar/parsing
mizar-items-srcdir = /Users/alama/sources/mizar/mizar-items
mizar-items-xsldir = $(mizar-items-srcdir)/src/xslt

pp-stylesheet = $(pp-srcdir)/pp.xsl
wrm-stylesheet = $(mizar-items-xsldir)/wrm.xsl
expand-canceled-stylesheet = $(mizar-items-xsldir)/expand-canceled.xsl
split-stylesheet = $(mizar-items-xsldir)/split.xsl
factor-proofs-stylesheet = $(mizar-items-xsldir)/factor-proofs.xsl
check-factorization-stylesheet = $(mizar-items-xsldir)/check-factorization.xsl
itemize-stylesheet = $(mizar-items-xsldir)/itemize.xsl

run-mizar-utility = ($1 -q -l $2 > /dev/null 2>&1) && (test -e $2.err) && (test ! -s $2.err)
run-mizar-utility-ignoring-err = ($1 -q -l $2 > /dev/null 2>&1); echo > $2.err; true
accom = $(call run-mizar-utility,accom,$1)
verifier = $(call run-mizar-utility,verifier,$1)
checker = $(call run-mizar-utility,verifier -c,$1)
dellink = $(call run-mizar-utility,dellink,$1)
makeenv = $(call run-mizar-utility,makeenv,$1)
analyzer = $(call run-mizar-utility,verifier -a,$1)
msplit = $(call run-mizar-utility,msplit,$1)
mglue = $(call run-mizar-utility,mglue,$1)

# unhereby exits uncleanly and may leave behind a non-empty .err file
# edtfile normally exits cleanly, but it seems that in combination
# with unhereby, edtfile might leave behind a non-empty .err file even
# if it worked correctly.
edtfile = $(call run-mizar-utility-ignoring-err,edtfile,$1)
unhereby = $(call run-mizar-utility-ignoring-err,unhereby,$1)

xsltproc = (xsltproc --output $3 $2 $1) || (test -e $3 && rm -f $3; false)

all:
	# nothing to do

%.wrx: %.miz $(wrm-stylesheet)
	$(call accom,$*)
	$(call unhereby,$*)
	$(call edtfile,$*)
	test -e $*.\$$-\$$
	mv $*.\$$-\$$ $*.miz
	$(call dellink,$*)
	$(call edtfile,$*)
	test -e $*.\$$-\$$
	mv $*.\$$-\$$ $*.miz
	$(call analyzer,$*)
	test -e $*.msx
	$(call xsltproc,$*.msx,$(wrm-stylesheet),$*.wrx)

%.wrm : %.wrx $(pp-stylesheet)
	$(call xsltproc,$*.wrx,$(pp-stylesheet),$@)

%.tpr %.evd : %.miz
	$(call msplit,$*)

%.msx : %.miz
	$(call analyzer,$*)

%.miz :
	# nothing to do

%.xml: %.miz
	$(call makeenv,$*)
	$(call analyzer,$*)

%.verifier-stamp: %.xml
	$(call checker,$*)
	touch $*.verifier-stamp

%.no-canceled.xml : %.wrx $(expand-canceled-stylesheet)
	$(call xsltproc,$<,$(expand-canceled-stylesheet),$@)

%.no-canceled : %.no-canceled.xml $(pp-stylesheet)
	$(call xsltproc,$<,$(pp-stylesheet),$@)

%.split.xml : %.no-canceled.xml $(split-stylesheet)
	$(call xsltproc,$<,$(split-stylesheet),$@)

%.split : %.split.xml $(pp-stylesheet)
	$(call xsltproc,$<,$(pp-stylesheet),$@)

%.factored.xml : %.split.xml $(factor-proofs-stylesheet)
	$(call xsltproc,$<,$(factor-proofs-stylesheet),$@)
	xsltproc $(check-factorization-stylesheet) $@

%.factored : %.factored.xml $(pp-stylesheet)
	$(call xsltproc,$<,$(pp-stylesheet),$@)

%.itemized.xml : %.factored %.tpr $(itemize-stylesheet)
	cp $*.factored $*.tpr
	$(call mglue,$*)
	$(call accom,$*)
	$(call analyzer,$*)
	$(call xsltproc,$*.msx,$(itemize-stylesheet),$@)

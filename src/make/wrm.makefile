.SECONDARY:

pp-srcdir = /Users/alama/sources/mizar/parsing
mizar-items-srcdir = /Users/alama/sources/mizar/mizar-items
mizar-items-xsldir = $(mizar-items-srcdir)/src/xslt
pp-stylesheet = $(pp-srcdir)/pp.xsl
wrm-stylesheet = $(mizar-items-xsldir)/wrm.xsl

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
	xsltproc --output $*.wrx $(wrm-stylesheet) $*.msx

%.wrm : %.wrx $(pp-stylesheet)
	xsltproc --output $@ --stringparam suppress-environment '1' $(pp-stylesheet) $*.wrx

%.tpr %.evd : %.miz
	$(call msplit,$*)

%.msx : %.miz
	$(call analyzer,$*)

%.miz : %.miz.dellink
	cp $< $@

%.miz.dellink :
	accom -l $*
	dellink -l $*
	edtfile -l $*
	cp $*.$-$ $*.miz.dellink

%.xml: %.miz
	$(call makeenv,$*)
	$(call analyzer,$*)

%.verifier-stamp: %.xml
	$(call checker,$*)
	touch $*.verifier-stamp

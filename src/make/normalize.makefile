.SECONDARY:

include config.makefile
include mizar.makefile
include xslt.makefile

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

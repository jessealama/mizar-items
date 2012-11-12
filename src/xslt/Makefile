sheets = $(basename $(wildcard *.xsltxt))
mml = $(shell cat $(MIZFILES)/mml.lar)
mml-100 = $(wordlist 1,100,$(mml))
mml-10 = $(wordlist 1,10,$(mml))

xsltxts = $(addsuffix .xsltxt,$(sheets))
xsls    = $(addsuffix .xsl,$(sheets))
editable-files = $(xsltxts) Makefile

emacs-backups = $(addsuffix ~,$(editable-files))

# functions
xslize = java -jar xsltxt.jar toXSL $(1) $(2)
xslize-or-kill = $(call xslize,$(1),$(2)) || (rm -f $(2); false)

.PHONY: all clean wrms

all: $(xsls)

%.xsl: %.xsltxt
	$(call xslize-or-kill,$*.xsltxt,$*.xsl)

clean:
	find . -maxdepth 1 -type f -name "*.xsl" -delete
	find . -maxdepth 1 -type f -name "*~" -delete
	find . -maxdepth 1 -name "*.miz" -exec basename {} .miz ';' | parallel rm -f "{}.*"

%.miz %.wsx %.wsm %.tpr %.evd:
	cp $(MIZFILES)/mml/$*.miz .
	accom -l -q $* > /dev/null 2>&1
	msplit -l -q $*
	wsmparser -l -q $* > /dev/null 2>&1
	msmprocessor -l -q $* > /dev/null 2>&1
	cp $*.msm $*.tpr
	touch $*.evd
	mglue -l -q $* > /dev/null 2>&1
	accom -l -q $* > /dev/null 2>&1
	wsmparser -l -q $* > /dev/null 2>&1
	msmprocessor -l -q $* > /dev/null 2>&1

%.xml: %.miz
	makeenv -l $* > /dev/null 2>&1
	verifier -l $* > /dev/null 2>&1 || (errflag $*; addfmsg $* $(MIZFILES)/mizar; false)

%.wrx: %.wsx wrm.xsl
	xsltproc --output $*.wrx wrm.xsl $*.wsx

%.wrm: %.wrx /Users/alama/sources/mizar/parsing/pp.xsl
	xsltproc --output $*.wrm --stringparam suppress-environment '1' /Users/alama/sources/mizar/parsing/pp.xsl $*.wrx

wrms: $(addsuffix .wrm,$(mml))

wrms-100: $(addsuffix .wrm,$(mml-100))
tpr-100: $(addsuffix .tpr,$(mml-100))

wrms-10: $(addsuffix .wrm,$(mml-10))
tpr-10: $(addsuffix .tpr,$(mml-10))

xmls-10: $(addsuffix .xml,$(mml-10))
xmls-100: $(addsuffix .xml,$(mml-100))

clean:

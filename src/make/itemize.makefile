include config.makefile mizar.makefile

.SECONDARY:

%.dir :
	test ! -d $*.dir
	mkdir $*.dir
	test -e $$MIZFILES/mml/$*.miz
	cp $$MIZFILES/mml/$*.miz $*.dir
	$(MAKE) -C $*.dir -f $(normalize-makefile) $*.factored
	$(call msplit,$*.dir/$*)
	cp $*.dir/$*.factored $*.dir/$*.tpr
	$(call mglue,$*.dir/$*)
	$(call verifier,$*.dir/$*)
	mkdir $*.dir/prel
	mkdir $*.dir/text
	$(MAKE) -C $*.dir -f $(itemize-makefile) items.makefile article=$*
	$(MAKE) -C $*.dir -f items.makefile all

items.makefile :
	test -e $(article).msx
	(xsltproc $(itemize-stylesheet) $(article).msx > $(article).itemized.xml) || (rm -f $(article.itemized.xml); false)
	(xsltproc $(itemize-makefile-stylesheet) $(article).itemized.xml > $@) || (rm -f $@; false)

%.deps : %.dir
	# does this look like a local db?
	test -d $*.dir/prel
	test -d $*.dir/text
	test -x $(itemized-article-dependencies)
	($(itemized-article-dependencies) $< > $@) || (rm -f $@; false)

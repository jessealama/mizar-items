.PHONY: ckbs

%.itemized : %.miz
	mkdir $@
	cp $< $@
	$(MAKE) -C $@ ckbs

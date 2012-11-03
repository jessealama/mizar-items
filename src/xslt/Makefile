sheets = $(basename $(wildcard *.xsltxt))

xsltxts = $(addsuffix .xsltxt,$(sheets))
xsls    = $(addsuffix .xsl,$(sheets))
editable-files = $(xsltxts) Makefile

emacs-backups = $(addsuffix ~,$(editable-files))

# functions
xslize = java -jar xsltxt.jar toXSL $(1) $(2)
xslize-or-kill = $(call xslize,$(1),$(2)) || (rm -f $(2); false)

.PHONY: all clean

all: $(xsls)

%.xsl: %.xsltxt
	$(call xslize-or-kill,$*.xsltxt,$*.xsl)

clean:
	rm -f $(xsls)
	rm -f $(emacs-backups)

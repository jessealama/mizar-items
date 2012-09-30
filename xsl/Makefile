JAVA = java
XSLTXT = xsltxt.jar
CAT = cat

sheets = addabsrefs \
         bogus \
         conditions-and-properties \
         constructors-with-abstractness \
         constructors \
         contains-toplevel-private-function \
         contains-toplevel-private-predicate \
         contains-toplevel-type-changing-statements \
         count-toplevel \
         delete-abstractness \
         delete-elements \
         delete-properties \
         keep-elements \
         dependencies \
         die \
         elicit-correctness-conditions \
         evl2environ \
         extend-evl \
         fold-reservations \
         fragments-of-lemmas \
         free-variables \
         id \
         implicitly-qualified \
         inferred-constructors \
         itemize \
         lemma-deps-non-absolute \
         lemma-deps \
         list-properties \
         no-attributes \
         non-scheme-uses-scheme \
         paths \
         prefer-environment \
         print-evl \
         print-toplevel \
         promotable-constructors \
         promote-lemmas \
         promote \
         promoted-constructors \
         propertied-constructors \
         properties-of-constructor \
         properties-of-constructors \
         remove-properties \
         resolve-references \
         rewrite-aid \
         skip-non-ultimate-proofs \
         skip-proofs-preceding \
         split-reservations \
         split \
         strings \
         strip-bogus-structure-registration \
         strip-definition \
         strip-prop \
         strip-property \
         strip-registration \
         structure-dependencies \
         tokens \
         toplevel-choice \
         toplevel-constant-definition \
         toplevel-dellink \
         toplevel-diffuse \
         toplevel-items \
         toplevel-private-functions \
         toplevel-private-predicates-and-functions \
         toplevel-propositions \
         toplevel-type-changing-statements \
         trim-properties-and-conditions \
         truncate \
         unpromoted-constructors \
         wsm \
         uninteresting-attributes

xsltxts = $(addsuffix .xsltxt,$(sheets))
xsls    = $(addsuffix .xsl,$(sheets))
editable-files = $(xsltxts) Makefile

emacs-backups = $(addsuffix ~,$(editable-files))

.PHONY: all clean

all: $(xsls)

%.xsl: %.xsltxt
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt $*.xsl || (rm -f $*.xsl; false)

clean:
	rm -f $(xsls)
	rm -f $(emacs-backups)

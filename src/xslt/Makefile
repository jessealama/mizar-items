sheets = addabsrefs \
         bogus \
         canceled-deftheorems \
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
         expand-canceled \
         keep-elements \
         dependencies \
         die \
         elicit-correctness-conditions \
         evl2environ \
         extend-evl \
         factor-proofs \
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
         mptp-fraenkels \
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
         redefined-constructors \
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

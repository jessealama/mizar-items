.PHONY: all clean check

shell-scripts = check-itemiziation \
                check-newparser \
                check-num-xmls \
                check-text-dir-exists \
                exec-in-dir \
                fix-by-and-from \
                itemize \
                items-needed-for-item \
                listvoc \
                position-of-broken-in-mmllar \
                property-statistics \
                regen-html \
                timed-quiet-verify \
                verify-itemization \
                verify-quietly
perl-scripts = absolutize \
               article-mptp \
               author-title \
               brutalize \
               cluster-fragment-to-item \
               compare-tables \
               constr_reduce \
               dependencies \
               depgraph \
               env \
               equivalent-miz-xml \
               expand-canceled \
               export-by-problem \
               fix-constructors \
               inferred-constructors \
               item-depgraph \
               itemize \
               itemized-article-dependencies \
               items-to-mptp \
               items \
               lemma-unpromotable-constructors \
               map-ckbs \
               minimal \
               minimize-abstractness \
               minimize-constructors \
               minimize-internally \
               minimize-itemized-article \
               minimize-lemmas \
               minimize-properties \
               minimize-requirements \
               miz_item_deps_bf \
               miz_item_deps_bf_tarski \
               mptp-table-for-article \
               mptp \
               needed-constructors \
               needed-properties-for-fragment \
               needed-properties \
               null \
               promotable-constructors \
               promote-constructors \
               prune-theorems \
               reduce-item \
               reduce-parallel \
               reduce-sketchy \
               reduce-tarski \
               reduce \
               selectrefs \
               split \
               table-info \
               tarski-line \
               tc \
               to-mizar-ns \
               to-mptp-ns \
               unpromotable-constructors \
               xml-fancy \
               xml-to-mptp

shs = $(addsuffix .sh,$(shell-scripts))
pls = $(addsuffix .pl,$(perl-scripts))
editable-files = $(shs) $(pls)
emacs-backups = $(addsuffix ~,$(editable-files))

all: check

clean:
	rm -f $(emacs-backups)

check:
	ok="1"; \
        for script in $(shs); do bash -n $$script || ok="0"; done; \
        for script in $(pls); do perl -c $$script || ok="0"; done; \
        [ "$$ok" = "1" ];

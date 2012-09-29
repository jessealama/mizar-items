.PHONY: all clean check

scripts := absolutize \
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

pls := $(addsuffix .pl,$(scripts))
editable-files := $(pls) Makefile
emacs-backups := $(addsuffix ~,$(editable-files))

all:

clean:
	rm -f $(emacs-backups)

check:
	ok=1; for pl in $(pls); do perl -c $$pl || ok=0; done; [ "$$ok" -eq "1" ];

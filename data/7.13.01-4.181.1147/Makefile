.PHONY: all 100 check upload

mml = $(shell cat mml.lar)

dependency-tables = $(addsuffix .deps,$(mml))
maps = $(addsuffix .map,$(mml))

first-10-mml = $(wordlist 1,10,$(mml))
first-100-mml = $(wordlist 1,100,$(mml))
first-100-dependency-tables = $(wordlist 1,100,$(dependency-tables))
first-10-dependency-tables = $(wordlist 1,10,$(dependency-tables))
first-100-maps = $(addsuffix .map,$(first-100-mml))
first-10-maps = $(addsuffix .map,$(first-10-mml))
first-10-lemmas = $(addsuffix .lemmas,$(first-10-mml))
first-100-lemmas = $(addsuffix .lemmas,$(first-100-mml))

# functions
sort-table = ../../bin/sort-dependency-table.pl --fragment-table=$(3) --lar=mml.lar < $(1) > $(2)
sort-table-or-kill = ($(call sort-table,$(1),$(2),$(3))) || (rm -f $(2); false)

all:
	# no default action yet

%.deps:
	../../bin/itemized-article-dependencies.pl $* > $*.deps;

%.lemmas: %.map
	+make -C MPTP2 $*.tptp
	../../bin/lemmas.pl $*.map MPTP2/$*.tptp > $*.lemmas

lemma-map.10: $(first-10-lemmas)
	cat $(first-10-lemmas) > lemma-map.10

lemma-map.100: $(first-100-lemmas)
	cat $(first-100-lemmas) > lemma-map.100

100: dependencies.100

10: dependencies.10

item-fragment-table.100: $(first-100-maps)
	cat $(first-100-maps) | sed -e 's/ => / /' > item-fragment-table.100

item-fragment-table.10: $(first-10-maps)
	cat $(first-10-maps) | sed -e 's/ => / /' > item-fragment-table.10

dependencies.10: $(first-10-dependency-tables) item-fragment-table.10
	cat $(first-10-dependency-tables) > dependencies.10.1
	$(call sort-table,dependencies.10.1,dependencies.10,item-fragment-table.10)
	rm -f dependencies.10.1

dependencies.10.mptp: dependencies.10
	../../bin/mptp.pl --lemma-map=lemma-map.10 --mptp-axioms=MPTP2/00allmmlax < dependencies.10 > dependencies.10.mptp || rm -f dependencies.10.mptp

dependencies.100.mptp: dependencies.100 lemma-map.100
	+make -C MPTP2 fraenkel-dependencies.100
	../../bin/mptp.pl --lemma-map=lemma-map.100 --mptp-axioms=MPTP2/00allmmlax < dependencies.100 > dependencies.100.mptp || rm -f dependencies.100.mptp
	../../bin/insert-fraenkels.pl dependencies.100.mptp MPTP2/fraenkel-dependencies.100 > dependencies.100.mptp1
	mv dependencies.100.mptp1 dependencies.100.mptp
	../../bin/insert-fraenkels.pl dependencies.100.mptp MPTP2/choice-terms > dependencies.100.mptp1
	mv dependencies.100.mptp1 dependencies.100.mptp
	../../bin/table-info.pl dependencies.100.mptp implicitly-independent-items > dependencies.100.mptp.implicitly-independent
	../../bin/insert-fraenkels.pl dependencies.100.mptp dependencies.100.mptp.implicitly-independent > dependencies.100.mptp1
	mv dependencies.100.mptp1 dependencies.100.mptp

dependencies.100.mptp.gz: dependencies.100.mptp
	rm -f dependencies.100.mptp.gz
	gzip dependencies.100.mptp

upload: dependencies.100.mptp.gz
	scp dependencies.100.mptp.gz alama@mizar.cs.ualberta.ca:~/public_html/7.13.01-4.181.1147/

dependencies.100.mptp.items: dependencies.100.mptp
	cat dependencies.100.mptp | tr ' ' '\n' | sort --unique > dependencies.100.mptp.items

dependencies.100: $(first-100-dependency-tables) item-fragment-table.100
	cat $(first-100-dependency-tables) > dependencies.100.1
	$(call sort-table-or-kill,dependencies.100.1,dependencies.100,item-fragment-table.100)
	# rm -f dependencies.100.1

dependencies.tc: dependencies
	../../bin/table-info.pl transitive dependencies > dependencies.tc.1
	../../bin/sort-dependency-table.pl < dependencies.tc.1 > dependencies.tc

dependencies.mptp: dependencies
	../../bin/mptp.pl < dependencies > dependencies.mptp

dependencies.mptp.tc: dependencies.tc
	../../bin/mptp.pl < dependencies.tc > dependencies.tc.mptp

check: dependencies dependencies.tc
	../../bin/table-info.pl structural-check dependencies
	../../bin/table-info.pl structural-check dependencies.tc

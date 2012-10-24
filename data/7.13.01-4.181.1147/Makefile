.PHONY: all 100

mml = $(shell cat mml.lar)

dependency-tables = $(addsuffix .deps,$(mml))
maps = $(addsuffix .map,$(mml))

first-100-dependency-tables = $(wordlist 1,100,$(dependency-tables))
first-10-dependency-tables = $(wordlist 1,10,$(dependency-tables))
first-100-maps = $(wordlist 1,100,$(maps))
first-10-maps = $(wordlist 1,10,$(maps))

# functions
sort-table = ../../bin/sort-dependency-table.pl --fragment-table=$(3) --lar=mml.lar < $(1) > $(2)
sort-table-or-kill = ($(call sort-table,$(1),$(2),$(3))) || (rm -f $(2); false)

all:
	# no default action yet

%.deps:
	../../bin/itemized-article-dependencies.pl $* > $*.deps;

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

dependencies.100: $(first-100-dependency-tables) item-fragment-table.100
	cat $(first-100-dependency-tables) > dependencies.100.1
	$(call sort-table-or-kill,dependencies.100.1,dependencies.100,item-fragment-table.100)
	rm -f dependencies.100.1

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

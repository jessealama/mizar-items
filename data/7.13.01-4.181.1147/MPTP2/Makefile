.PHONY: all clean

mml = hidden tarski $(shell cat ../mml.lar)
mml-100 = $(wordlist 1,100,$(mml))
lem2s = $(addsuffix .lem2,$(mml))
tptp-lemmas = $(addsuffix .tptp,$(mml))

%.tptp: %.lem2 utils.pl
	swipl -l utils.pl -t "mptp2tptp('$*.lem2',[opt_NO_FRAENKEL_CONST_GEN],'$*.tptp')." > /dev/null 2>&1

00allmmlax.100.xml: 00allmmlax.100
	tptp4X -umachine -fxml 00allmmlax.100 > 00allmmlax.100.xml

fraenkel-dependencies.100: 00allmmlax.100.xml
	make -C ../../../src/xslt mptp-fraenkels.xsl
	xsltproc ../../../src/xslt/mptp-fraenkels.xsl 00allmmlax.100.xml > fraenkel-dependencies.100.1
	../../../bin/table-info.pl fraenkel-dependencies.100.1 print > fraenkel-dependencies.100

00allmmlax.100: 00allmmlax
	for article in $(mml-100); \
          do egrep "^fof[(][^ ]+[_]$$article[,]" 00allmmlax; \
        done > 00allmmlax.100

00allmmlax.items: 00allmmlax
	cut -f 1 -d ',' 00allmmlax | cut -f 2 -d '(' | sort --unique > 00allmmlax.items

00allmmlax.100.items: 00allmmlax.100
	cut -f 1 -d ',' 00allmmlax.100 | cut -f 2 -d '(' | sort --unique > 00allmmlax.100.items

all: $(tptp-lemmas)

clean:
	rm -f $(tptp-lemmas)

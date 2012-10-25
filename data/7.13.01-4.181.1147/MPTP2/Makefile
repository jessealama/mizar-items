.PHONY: all clean

mml = hidden tarski $(shell cat ../mml.lar)
lem2s = $(addsuffix .lem2,$(mml))
tptp-lemmas = $(addsuffix .tptp,$(mml))

%.tptp: %.lem2 utils.pl
	swipl -l utils.pl -t "mptp2tptp('$*.lem2',[opt_NO_FRAENKEL_CONST_GEN],'$*.tptp')." > /dev/null 2>&1

all: $(tptp-lemmas)

clean:
	rm -f $(tptp-lemmas)

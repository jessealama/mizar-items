export_assumptions(Stream,[]) :- close(Stream).
export_assumptions(Stream,[Assumption|MoreAssumptions]) :-
	fof(Assumption,_,Formula,_,_),
	write(Stream,Formula),
	nl(Stream),
	export_assumptions(Stream,MoreAssumptions).
export_by_step(ByStep) :-
	fof(ByStep,_,Conjecture,inference(mizar_by,_,Assumptions),_),
	open(ByStep,write,ProblemStream),
	export_assumptions(Stream,Assumptions).

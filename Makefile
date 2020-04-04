
m: main.hs Makefile Params.hs
	ghc -dynamic -threaded $(filter %.hs, $^) -o $@

tstfn: tstfn.hs
	ghc $< -o $@

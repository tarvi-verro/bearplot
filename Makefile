
m: main.hs Makefile
	ghc -dynamic -threaded $(filter %.hs, $^) -o $@

tstfn: tstfn.hs
	ghc $< -o $@

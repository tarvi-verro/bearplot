
m: main.hs StreamInput.hs Makefile Params.hs
	ghc -dynamic -threaded $(filter %.hs, $^) -o $@

tstfn: tstfn.hs
	ghc $< -o $@

plot-tstfn: m tstfn
	./tstfn | ./m

plot-mouse: m
	(while true; do \
		xdotool getmouselocation | awk '{ sub(/x:/, "", $$1); sub(/y:/, "", $$2); print $$1 / 192.0,  $$2 / 108.0 }'; \
		sleep .03; \
	done) | ./m

.PHONY: plot-tstfn plot-mouse


bearplot: main.hs StreamInput.hs Makefile Params.hs
	ghc -dynamic -threaded $(filter %.hs, $^) -o $@

tstfn: tstfn.hs
	ghc $< -o $@

plot-tstfn: bearplot tstfn
	./tstfn | ./bearplot

plot-mouse: bearplot
	(while true; do \
		xdotool getmouselocation | awk '{ sub(/x:/, "", $$1); sub(/y:/, "", $$2); print $$1 / 192.0,  $$2 / 108.0 }'; \
		sleep .03; \
	done) | ./bearplot

plot-sys: bearplot
	{ while true; do \
		{ \
		top -bn1 | grep "Cpu(s)" | grep -o '[0-9.]\+ id' | awk '{ print (100-$$1)/10 }'; \
		free --si | head -n2 | tail -n1 | awk '{ print 10 - $$7 / $$2 * 10.0 }'; \
		} | xargs echo; \
		sleep 0.5; \
	done } | ./bearplot

.PHONY: plot-tstfn plot-mouse plot-sys

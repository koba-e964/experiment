run: QSPerf
	./QSPerf 800000 +RTS -K80m -RTS
QSPerf: QSPerf.hs
	ghc $< -O3 -Wall -fforce-recomp -rtsopts


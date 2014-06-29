EXEC = QSPerf
run: $(EXEC)
	./QSPerf 800000 +RTS -K80m -RTS
$(EXEC): QSPerf.hs
	ghc $< -O3 -Wall -fforce-recomp -rtsopts -o $(EXEC)
clean:
	rm -rf *.hi *.o $(EXEC)


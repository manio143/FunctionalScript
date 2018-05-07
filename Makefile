all: interpreter clean

interpreter: *.hs
	ghc --make Main.hs -o interpreter

.PHONY: clean
clean:
	@rm -f *.hi *.o

.PHONY: clean-all
clean-all: clean
	@rm -f interpreter
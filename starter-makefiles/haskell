GHC = ghc
SOLUTIONS=$(wildcard [a-z]*.hs)
SOLVERS=$(SOLUTIONS:.hs=.bin)

all: $(SOLVERS)

%.bin: %.hs
	$(GHC) -o $@ --make $<

test:
	@echo placeholder

love:
	@echo "not war"

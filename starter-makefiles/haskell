NAME = solution.bin
SRC = solution.hs
BIN = $(NAME)

GHC = ghc
GHC_OPTIMISATION = -O2
GHC_FLAGS = 	-fwarn-unused-imports \
				-fforce-recomp \
				-fwarn-incomplete-patterns \
				-Wall \
				-fno-warn-unused-do-bind \
				-fno-warn-name-shadowing
GHC_OPTIONS = 	-threaded \
				$(GHC_OPTIMISATION) \
				$(GHC_FLAGS)


SRC_DIR = src

all: bin

bin:
	$(GHC) $(GHC_OPTIONS) -o $(BIN) --make $(SRC)

TESTSUITE = testsuite.bin
TEST_SRC = Test.hs

test: testsuite
	./$(TESTSUITE)

testsuite:
	$(GHC) -o $(TESTSUITE) $(TEST_SRC) --make -main-is Test.test
	
	
DIRTY_EXT = *.o *.hi *.bin

clean:
	rm -f $(BIN) $(DIRTY_EXT)

love:
	@echo "not war"

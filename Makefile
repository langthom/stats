#############################################################
# Makefile for "Stats" - The Statistical Commandline Software
#############################################################

CC     = ghc
EXEC   = stats
SOURCE = StatsMain.hs


# Build that whole thing
install:
	cabal configure
	cabal build
	cabal install


# Compiling and linking everything up
#
# Note that the GHC will automatically
# Bind in other Modules, so a single
# command is completely enough
all:
	$(CC) --make -o $(EXEC) $(SOURCE)


clean:
	@echo 'Cleaning up ...'
	@rm $(EXEC)
	@echo 'Removing executable ... done.'
	@rm *.hi *.o 
	@echo 'Removing object and data files ... done.'
	@echo 'DONE.'

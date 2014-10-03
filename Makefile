#############################################################
# Makefile for "Stats" - The Statistical Commandline Software
#############################################################

CC     = ghc
EXEC   = stats
SOURCE = StatsMain.hs


# Build locally:
#
# Compiling and linking everything up
#
# Note that the GHC will automatically
# Bind in other Modules, so a single
# command is completely enough
all:
	$(CC) --make -o $(EXEC) $(SOURCE)


# Install that thing globally
install:
	cabal configure
	cabal build
	cabal install


# create a zipped tarball:
package:
	runhaskell Setup.lhs sdist


# display the help message
help:
	@echo 'Makefile help:'
	@echo
	@echo 'make:         Build program locally'
	@echo 'make help:    Display this message'
	@echo 'make install: Install globally'
	@echo 'make package: Create a zipped tarball'


clean:
	@echo 'Cleaning up ...'
	@rm $(EXEC)
	@echo 'Removing executable ... done.'
	@rm *.hi *.o 
	@echo 'Removing object and data files ... done.'
	@echo 'DONE.'

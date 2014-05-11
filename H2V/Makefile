PROF_FLAGS=-rtsopts -prof -auto-all

#GHC Flags:
#	ScopedTypeVariables - used to constrain type within do blocks (prevents combinatorial number of error messages)
#	DeriveDataTypeable - needed for custom exceptions
#	hide-package - both mtl and monads-tf define Control.Monad.State, so we need to hide the one we don't use
HFLAGS=-XScopedTypeVariables -XDeriveDataTypeable -hide-package=monads-tf

all:
	ghc --make main.hs -outputdir build -o H2V $(HFLAGS) $(PROF_FLAGS)

clean:
	rm -f build/* H2V

test: all
	clear
	./H2V test.hs
	echo ================================================================================
	cat test.gv
	dot -Tpng test.gv > test.png
	xdg-open test.png

debug: all test

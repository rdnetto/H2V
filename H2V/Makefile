PROF_FLAGS=-rtsopts -prof -auto-all

#GHC Flags:
#	ScopedTypeVariables - used to constrain type within do blocks (prevents combinatorial number of error messages)
#	DeriveDataTypeable - needed for custom exceptions
#	hide-package - both mtl and monads-tf define Control.Monad.State, so we need to hide the one we don't use
HFLAGS=-XScopedTypeVariables -XDeriveDataTypeable -hide-package=monads-tf

all:
	ghc --make main.hs -outputdir build -o H2V $(HFLAGS)

clean:
	rm -f build/* H2V

test: testG testV

testD: all
	clear
	./H2V -d test.hs

testG: all
	clear
	./H2V -g test.hs
	#================================================================================
	dot -Tpng test.gv > test.png

testV: all
	clear
	./H2V -v test.hs
	cp test.v ../scratch/verilog-test/test.v

debug: all test

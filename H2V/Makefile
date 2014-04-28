all:
	ghc --make main.hs -outputdir build -o H2V -rtsopts -prof -auto-all -hide-package=monads-tf

clean:
	rm -f build/* H2V

test:
	clear
	./H2V test.hs
	echo ================================================================================
	cat test.gv
	dot -Tpng test.gv > test.png
	xdg-open test.png

debug: all test

all:
	bnfc --latex MajaC.cf
	pdflatex *.tex
	bnfc -haskell MajaC.cf
	happy -gca ParMajaC.y
	alex -g LexMajaC.x
	ghc --make -O2 MajaCInterpreter.hs -o MajaCInterpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMajaC.* LexMajaC.* ParMajaC.* LayoutMajaC.* SkelMajaC.* PrintMajaC.* TestMajaC.* AbsMajaC.* TestMajaC ErrM.* SharedString.* ComposOp.* MajaC.dtd XMLMajaC.* Makefile*
	


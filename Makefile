haskell:
	ghc -O2 cp1415t -rtsopts -threaded

latex:
	lhs2TeX cp1415t.lhs > cp1415t.tex
	pdflatex cp1415t.tex
	pdflatex cp1415t.tex

clean:
	-rm *.o *.hi cp1415t
	-rm *.aux *.idx *.log *.out *.ptb *.tex *.toc



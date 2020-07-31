.POSIX:
GHC_FLAGS=-dynamic

dir: dir.hs
	ghc $(GHC_FLAGS) -o dir dir.hs
doc: dir.tex
	pdflatex dir.tex
dir.tex: dir.lhs
	lhs2TeX -odir.tex dir.lhs
dir.lhs: before.lhs dir.hs after.lhs
	cat before.lhs dir.hs after.lhs >dir.lhs
before.lhs: mkbeforeafter.sh
	sh mkbeforeafter.sh
after.lhs: mkbeforeafter.sh
	sh mkbeforeafter.sh
clean:
	$(RM) dir *.lhs *.tex *.aux *.ptb *.pdf *.o *.hi *.log

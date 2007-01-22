LATEX=latex
BIBTEX=bibtex

SOURCES = f2j_ug.tex title.tex

f2j_ug.ps: f2j_ug.dvi

f2j_ug.dvi: $(SOURCES)
	$(LATEX) f2j_ug.tex 
#	$(BIBTEX) f2j_ug
#	$(LATEX) f2j_ug.tex
#	$(LATEX) f2j_ug.tex

f2j_ug.ps: f2j_ug.dvi
	dvips f2j_ug.dvi -o f2j_ug.ps

f2j_ug.pdf: f2j_ug.ps
	ps2pdf f2j_ug.ps

clean:
	rm -f f2j_ug.dvi f2j_ug.ps *.aux *.log *.out

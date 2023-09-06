default:
	nix-shell --pure --run "make september6.pdf"

september6.tex: september6.org
	emacs --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-beamer-export-to-latex --kill

september6.pdf: september6.tex
	pdflatex september6
	bibtex september6
	pdflatex september6
	pdflatex september6

clean:
	rm -f *.aux *.nav *.tex *.toc *.snm *.pdf *.bbl *.blg *.bcf *.out *.log *~ *.vrb *.bib *.xml

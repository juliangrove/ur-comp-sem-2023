default:
	nix-shell --pure --run "make september11.pdf"

september11.tex: september11.org
	emacs --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-beamer-export-to-latex --kill

september11.pdf: september11.tex
	pdflatex september11
	bibtex september11
	pdflatex september11
	pdflatex september11

clean:
	rm -f *.aux *.nav *.tex *.toc *.snm *.pdf *.bbl *.blg *.bcf *.out *.log *~ *.vrb *.bib *.xml

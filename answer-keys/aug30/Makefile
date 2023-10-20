default:
	nix-shell --pure --run "make key.pdf"

key.tex: key.org setup.org
	emacs --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-latex-export-to-latex --kill

key.pdf: key.tex
	pdflatex key
	biber key
	pdflatex key

clean:
	rm -f *.aux *.tex *.pdf *.bbl *.blg *.bcf *.out *.log *.xml *~

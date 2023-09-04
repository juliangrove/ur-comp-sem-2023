default:
	nix-shell --pure --run "make haskell_intro_1.pdf"

haskell_intro_1.tex: haskell_intro_1.org
	emacs --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-beamer-export-to-latex --kill

haskell_intro_1.pdf: haskell_intro_1.tex
	pdflatex haskell_intro_1
	bibtex haskell_intro_1
	pdflatex haskell_intro_1
	pdflatex haskell_intro_1

clean:
	rm -f *.aux *.nav *.tex *.toc *.snm *.pdf *.bbl *.blg *.bcf *.out *.log *~

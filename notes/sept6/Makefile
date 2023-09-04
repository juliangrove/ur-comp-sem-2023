default:
	nix-shell --pure --run "make haskell_types_data_structures_variables.pdf"

haskell_types_data_structures_variables.tex: haskell_types_data_structures_variables.org
	emacs --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-beamer-export-to-latex --kill

haskell_types_data_structures_variables.pdf: haskell_types_data_structures_variables.tex
	pdflatex haskell_types_data_structures_variables
	bibtex haskell_types_data_structures_variables
	pdflatex haskell_types_data_structures_variables
	pdflatex haskell_types_data_structures_variables

clean:
	rm -f *.aux *.nav *.tex *.toc *.snm *.pdf *.bbl *.blg *.bcf *.out *.log *~

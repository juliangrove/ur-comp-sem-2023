default:
	nix-shell --pure --run "make images.pdf"

images.pdf: images.tex
	pdflatex images

clean:
	rm -f *.aux *.pdf *.bbl *.blg *.bcf *.out *.log *~ *.xml

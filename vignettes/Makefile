default:
	make barsurf.pdf
	make removeother

barsurf.pdf: barsurf.tex
	pdflatex barsurf.tex

removeother:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
	rm -f Rplots*

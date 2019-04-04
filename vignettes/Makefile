default:
	make barsurf.pdf
	make remove_other

barsurf.pdf: barsurf.tex
	pdflatex barsurf.tex

remove_other:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
	rm -f Rplots*

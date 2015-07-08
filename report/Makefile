.PHONY: report bon clean

all: report bon

report:
	latexmk -pdf -latexoption=-interaction=nonstopmode report

bon:
	latexmk -pdf -latexoption=-interaction=nonstopmode bon_specifications
	
toc: report
	gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=2 -dLastPage=3 -sOutputFile=toc.pdf report.pdf >& /dev/null

clean:
	git clean -fX

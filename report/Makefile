.PHONY: report bon clean report_short expert usability

all: report bon report_short expert usability

report:
	latexmk -pdf -latexoption=-interaction=nonstopmode report

report_short:
	latexmk -pdf -latexoption=-interaction=nonstopmode report_short

expert:
	latexmk -pdf -latexoption=-interaction=nonstopmode expert_statements_standalone

usability:
	latexmk -pdf -latexoption=-interaction=nonstopmode usability_report

bon:
	latexmk -pdf -latexoption=-interaction=nonstopmode bon_specifications
	
toc: report
	gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=2 -dLastPage=3 -sOutputFile=toc.pdf report.pdf >& /dev/null

clean:
	git clean -fX

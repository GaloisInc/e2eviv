.PHONY: report bon clean report_short expert usability

all: report bon report_short expert usability

report:
	latexmk -pdf -latexoption=-interaction=nonstopmode report && \
	mv report.pdf E2EVIV_full_report.pdf

report_short:
	latexmk -pdf -latexoption=-interaction=nonstopmode report_short && \
	mv report_short.pdf E2EVIV_nontechnical_audience_report.pdf

expert:
	latexmk -pdf -latexoption=-interaction=nonstopmode expert_statements_standalone && \
	mv expert_statements_standalone.pdf E2EVIV_expert_statements.pdf

usability:
	latexmk -pdf -latexoption=-interaction=nonstopmode usability_report && \
	mv usability_report.pdf E2EVIV_usability_report.pdf

bon:
	latexmk -pdf -latexoption=-interaction=nonstopmode bon_specifications && \
	mv bon_specifications.pdf E2EVIV_bon_specifications.pdf

toc: report
	gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=2 -dLastPage=3 -sOutputFile=toc.pdf report.pdf >& /dev/null

clean:
	git clean -fX

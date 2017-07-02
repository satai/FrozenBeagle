export TEXINPUTS=../tex//:

all: dist/prace.pdf

# LaTeX je potreba spustit nekolikrat, aby spravne spocital odkazy
dist/prace.pdf: prace.tex $(wildcard *.tex) literatura.bib prace.xmpdata
	pdflatex -output-directory=dist $<
	cd dist; cp ../bibtex.bib . ;  bibtex prace ; cd ..
	pdflatex -output-directory=dist $<
	pdflatex -output-directory=dist $<

clean:
	rm -f dist/*

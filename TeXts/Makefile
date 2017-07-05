export TEXINPUTS=../tex//:

all: dist/prace.pdf

# LaTeX je potreba spustit nekolikrat, aby spravne spocital odkazy
dist/prace.pdf: prace.tex $(wildcard *.tex) literatura.bib prace.xmpdata
	cp literatura.bib *.tex *.bst prace.xmpdata dist/
	pdflatex -output-directory=dist $<
	cd dist/; bibtex prace ; cd ..
	pdflatex -output-directory=dist $<
	pdflatex -output-directory=dist $<

clean:
	rm -f dist/*

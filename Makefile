all: dist/prace.pdf

# Vyroba PDF primo z DVI by byla prijemnejsi, ale vetsina verzi dvipdfm nici obrazky
# prace.pdf: prace.dvi
#	dvipdfm -o $@ -p a4 -r 600 $<

dist/prace.pdf: dist/prace.ps
	ps2pdf $< $@

dist/prace.ps: dist/prace.dvi
	dvips -o $@ -D600 -t a4 $<

# LaTeX je potreba spustit dvakrat, aby spravne spocital odkazy
dist/prace.dvi: prace.tex $(wildcard *.tex)
	cslatex -output-directory=dist $<
	cslatex -output-directory=dist $<

clean:
	rm -f dist/*

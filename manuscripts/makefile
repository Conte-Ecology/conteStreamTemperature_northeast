all: northeast_temperature_ms.pdf northeast_temperature_ms.doc northeast_temperature_ms.tex northeast_temperature_ms_bib.tex northeast_temperature_figures.pdf northeast_temperature_figures.tex

northeast_temperature_ms.doc: northeast_temperature_ms2.md northeast_temperature_refs.bib
	pandoc -H format.sty -V fontsize=12pt --bibliography northeast_temperature_refs.bib --csl=american-geophysical-union.csl northeast_temperature_ms2.md -o northeast_temperature_ms.doc

northeast_temperature_ms.pdf: northeast_temperature_ms2.md northeast_temperature_refs.bib
	pandoc -H format.sty -V fontsize=12pt --bibliography northeast_temperature_refs.bib --csl=american-geophysical-union.csl northeast_temperature_ms2.md -o northeast_temperature_ms.pdf

northeast_temperature_ms.tex: northeast_temperature_ms2.md northeast_temperature_refs.bib
	pandoc northeast_temperature_ms2.md -t latex -s -S --natbib --bibliography=northeast_temperature_refs.bib --csl=agujournal.cls -o northeast_temperature_ms.tex

northeast_temperature_ms_bib.tex: northeast_temperature_ms2.md northeast_temperature_refs.bib
	pandoc northeast_temperature_ms2.md --bibliography northeast_temperature_refs.bib --csl=american-geophysical-union.csl -o northeast_temperature_ms_bib.tex


northeast_temperature_figures.pdf: northeast_temperature_figures.md
	pandoc -H format.sty -V fontsize=12pt --csl=american-geophysical-union.csl northeast_temperature_figures.md -o northeast_temperature_figures.pdf

northeast_temperature_figures.tex: northeast_temperature_figures.md
	pandoc -H format.sty -V fontsize=12pt --csl=american-geophysical-union.csl northeast_temperature_figures.md -o northeast_temperature_figures.tex

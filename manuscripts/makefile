all: northeast_temperature_ms.pdf northeast_temperature_ms.doc

northeast_temperature_ms.doc: northeast_temperature_ms.md northeast_temperature_refs.bib
	pandoc -H format.sty -V fontsize=12pt --bibliography northeast_temperature_refs.bib --csl=american-geophysical-union.csl northeast_temperature_ms.md -o northeast_temperature_ms.doc

northeast_temperature_ms.pdf: northeast_temperature_ms.md northeast_temperature_refs.bib
	pandoc -H format.sty -V fontsize=12pt --bibliography northeast_temperature_refs.bib --csl=american-geophysical-union.csl northeast_temperature_ms.md -o northeast_temperature_ms.pdf

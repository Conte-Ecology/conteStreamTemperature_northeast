# Build LaTeX document for submission

Directions to convert markdown manuscript draft to Latex for submission to a journal. In this case, the American Geophysical Union (AGU) styles will be used for the journal *Water Resources Research*.

1. Run `make` in directory

  * Alt. Run: `pandoc northeast_temperature_ms2.md -t latex -s -S --natbib --bibliography=northeast_temperature_refs.bib --csl=agujournal.cls -o northeast_temperature_ms.tex`
  * This creates a latex document but is not sufficiently formatted for a journal

2. 
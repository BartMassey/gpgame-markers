# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

GENCODE = gpgame-markers.py
GENERATOR = python3 ./$(GENCODE)

.SUFFIXES: .svg .pdf

.svg.pdf:
	cairosvg -d 300 -o $*.pdf $*.svg

SVG = front.svg back.svg numbers.svg
PDF = front.pdf back.pdf numbers.pdf

gpgame-markers.zip: $(PDF)
	zip -r gpgame-markers.zip *.pdf

pdf: $(PDF)

svg: $(SVG)

$(SVG): $(GENCODE)
	for i in $(SVG); do $(GENERATOR) "`basename $$i .svg`" >$$i ; done

clean:
	-rm -f $(SVG) $(PDF) gpgame-markers.zip

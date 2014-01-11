# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.


.SUFFIXES: .svg .pdf

.svg.pdf:
	cairosvg -d 300 -o $*.pdf $*.svg

SVG = front.svg back.svg numbers.svg
PDF = front.pdf back.pdf numbers.pdf

pdf: $(PDF)

svg: $(SVG)

$(SVG): gpg-markers.py
	for i in $(SVG); do python3 ./gpg-markers.py "`basename $$i .svg`" >$$i ; done

clean:
	-rm -f $(SVG) $(PDF)

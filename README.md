# Glass Plate Game Markers
Copyright © 2014 Bart Massey

This package generates three files of printable SVG
useful for feeding to a laser cutter to make wooden markers
for the [Glass Plate Game](http://glassplategame.com).

The easiest way to get the files is to just run "make" on
the Makefile. This will produce PDF by default. If you just
want SVG or do not have "cairosvg", which is used to
generate the PDF from the SVG, say "make svg".

By default, the SVG generator used is one written in
Python 3. If you would prefer a Haskell version of the
generator, edit the `Makefile`; you must have GHC installed
(or some other Haskell compiler, in which case edit the
Makefile further).

The markers with the numbers can be cut from
the front: set the laser cutter so that the blue is a gentle
emboss and the black is a full cut.

The other two files comprise front and back of the other
markers. Print the front first so that you get the two black
alignment holes useful for cutting the back. I suggest
taping down a sheet of paper under the wood before cutting
the front, so that it will be punched at the alignment
holes. This will let you flip over the wood and line it up
with the holes in the paper before cutting the back.

This work (such as it is) is licensed under the "MIT
License". Please see the file COPYING in this distribution
for license terms.

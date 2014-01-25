# Glass Plate Game Markers
Copyright © 2014 Bart Massey

This package generates three files of printable SVG
useful for feeding to a laser cutter to make wooden markers
for the [Glass Plate Game](http://glassplategame.com).

The SVG generator is written in Python 3.  The easiest way
to get the files is to just run "make" on the Makefile. This
will produce PDF by default. If you just want SVG or do not
have "cairosvg", which is used to generate the PDF from the
SVG, say "make svg".

The markers with the numbers can be cut from the front: set
the laser cutter so that the black is a gentle raster emboss
and the yellow is a full vector cut. For our cutter, the
relevant parameters are:

  * Vector Current 100
  * Raster Power 70 Speed 80
  * Vector Power 100 Speed 75

The other two files comprise front and back of the other
markers. As with the numbers, use raster with the black and
vector with the yellow. The easiest way is probably to tape
down the stock and cut the front first, which will free up a
panel that you can flip over and drop back into the hole to
cut the back.

There are two other versions of this generator in the repo
that were written in Haskell, but maintaining generators in
parallel seemed like a lot of pointless work. Dig them out
if you like.

This work (such as it is) is licensed under the "MIT
License". Please see the file COPYING in this distribution
for license terms.

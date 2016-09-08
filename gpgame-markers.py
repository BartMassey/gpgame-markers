#!/usr/bin/python3
# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.
# Generate SVG for laser-cut Glass Plate Game markers

from sys import argv

# All dimensions are in units of mm

fs = 7.0        # nominal font height
fontDrop = 3.0  # XXX drop the font by this amount for vertical centering,
                # unless the SVG renderer supports alignment-baseline
                # or display-anchor
kerf = 1.0      # nominal "kerf" width (actually inter-marker spacing)
setPad = 4.0    # space between sets
dRegMark = 2.0  # length of registration mark
markerR = 6.5   # radius of marker
markerD = 2.0 * markerR   # diameter of marker

debugSpacing = False   # gives a green square useful in spacing checks

def usage():
    raise Exception("usage: gpgame-markers")

if len(argv) != 1:
    usage()

# make an n x n grid of circles
n = 5

numbers = 1
letters = 2

# Distance to the left edge of the nth marker.
def dSpacing(c):
    return c * (markerD + kerf)

# Emit the SVG for a set of markers.
def printSet(gx, gy):

    def printLabel(x, y, label):
        print(('<text x="%fmm" y="%fmm" ' + \
              'text-anchor="middle" font-family="sans-serif" ' + \
              'font-size="%fmm" fill="black">%s</text>') % \
                  (x + gx, y + gy + fontDrop, fs, label))

    def printCutCircle(x, y):
        print(('<circle cx="%fmm" cy="%fmm" r="%fmm" ' + \
              'stroke="yellow" fill="none" stroke-width="1px"/>') % \
                  (gx + x, gy + y, markerR))

    if debugSpacing:
        setSquare = dSpacing(n) - kerf
        print(('<rect x="%fmm" y="%fmm" width="%fmm" height="%fmm" ' + \
              'fill="none" stroke="green" stroke-width="1px"/>') % \
                  (gx, gy, setSquare, setSquare))

    i = 0
    for y in range(n):
        for x in range(n):
            cx = dSpacing(x) + markerR
            cy = dSpacing(y) + markerR
            label = str(i + 1)
            if label == '6' or label == '9':
                label += '.'
            if x != n - 1 or y != n - 1:
                printLabel(cx, cy, label)
            printCutCircle(cx, cy)
            i += 1

def printFrame(x, y, width, height):
    print(('<rect x="%fmm" y="%fmm" width="%fmm" height="%fmm" ' + \
           'fill="none" stroke="yellow" stroke-width="1px"/>') % \
              (x, y, width, height))

def printRegLine(x1, y1, x2, y2):
    print(('<line x1="%fmm" y1="%fmm" x2="%fmm" y2="%fmm" ' + \
          'stroke="black" stroke-width="1px"/>') % \
              (x1, y1, x2, y2))

def printRegMark(x, y, dx, dy):
    printRegLine(x, y, x, y + dy)
    printRegLine(x, y, x + dx, y)

def printRegMarks(x, y, width, height):
    printRegMark(x, y, dRegMark, dRegMark)
    printRegMark(x + width, y + height, -dRegMark, -dRegMark)

dFrame = dSpacing(n)
print('<svg width="16in" height="12in">')

frameWidth = dFrame + kerf
frameHeight = dFrame + kerf
for r in range(4):
    for c in range(5):
        gx = c * (frameWidth + setPad) + setPad
        gy = r * (frameHeight + setPad) + setPad
        setX = gx + kerf
        setY = gy + kerf
        printSet(setX, setY)
        printFrame(gx, gy, frameWidth, frameHeight)

print('</svg>')

#!/usr/bin/python3
# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.
# Generate SVG for laser-cut Glass Plate Game markers

from sys import argv

# All dimensions are in units of cm.

fs = 0.6        # nominal font height
kerf = 0.2      # nominal kerf width
pad = 0.1       # nominal doc edge pad width
dRegMark = 0.1  # nominal length of registration mark

def usage():
    raise Exception("usage: gpg-markers <front|back|numbers>")

if len(argv) != 2:
    usage()

numbers = 1
front = 2
back = 3

modes = [
    ("numbers", numbers),
    ("front", front),
    ("back", back) ]

mode = 0
for (name, val) in modes:
    if argv[1] == name:
        mode = val
        break
if mode == 0:
    usage()

# make an n x n grid of circles
if mode == numbers:
    n = 5
else:
    n = 4

def printLabel(x, y, label):
    print('<text x="%fcm" y="%fcm" text-anchor="middle" font-family="sans-serif" font-size="%fcm" fill="black">%s</text>' % (x, y + fs / 2.4, fs, label))

# How to get to the center of the nth marker.
# (But add the pad and kerf as needed.)
def dSpacing(c):
    return c * (1 + kerf)

dFrame = dSpacing(n) + kerf   # width and height of framing rectangle

def printFrame():
    print('<rect x="%gcm" y="%gcm" width="%gcm" height="%gcm" fill="none" stroke="yellow" stroke-width="1px"/>' % (pad, pad, dFrame, dFrame))

def printRegLine(x1, y1, x2, y2):
    print('<line x1="%fcm" y1="%fcm" x2="%fcm" y2="%fcm" stroke="black" stroke-width="1px"/>' % (x1, y1, x2, y2))

def printRegMark(x, y, dx, dy):
    printRegLine(x, y, x, y + dy)
    printRegLine(x, y, x + dx, y)

def printRegMarks():
    printRegMark(pad, pad, dRegMark, dRegMark)
    printRegMark(pad + dFrame, pad + dFrame, -dRegMark, -dRegMark)

def printCutCircle(x, y):
    print('<circle cx="%fcm" cy="%fcm" r="0.5cm" stroke="yellow" fill="none" stroke-width="1px"/>' % (x, y))

dPage = dFrame + 2 * pad
print('<svg width="%fcm" height="%fcm">' % (dPage, dPage))
if mode == front:
    printFrame()
elif mode == back:
    printRegMarks()

i = 0
for y in range(n):
    for x in range(n):
        cx = dSpacing(x) + kerf + pad + 0.5
        cy = dSpacing(y) + kerf + pad + 0.5
        if mode == numbers:
            label = str(i + 1)
            if label == '6' or label == '9':
                label += '.'
            printLabel(cx, cy, label)
            printCutCircle(cx, cy)
        elif mode == front:
            if x != n - 1 or y != n - 1:
                printLabel(cx, cy, 'O')
        elif mode == back:
            if x != 0 or y != n - 1:
                printLabel(cx, cy, 'N')
            printCutCircle(cx, cy)
        else:
            raise Exception("unknown mode")
        i += 1
print('</svg>')

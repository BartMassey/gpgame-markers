#!/usr/bin/python3
# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.
# Generate SVG for laser-cut Glass Plate Game markers

from sys import argv

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

n = 5   # make an n x n grid of circles
fs = 0.6   # nominal font height in cm

frontLabels = 'C'*25
backLabels = 'O'*17 + 'N'*8

def printLabel(x, y, label):
    print('<text x="%fcm" y="%fcm" text-anchor="middle" font-family="sans-serif" font-size="%fcm" fill="blue">%s</text>' % (x, y + fs / 2.4, fs, label))

def printAlignmentHole(x, y):
    print('<circle cx="%fcm" cy="%fcm" r="0.1cm" fill="black"/>' % (x, y))

def printCutCircle(x, y):
    print('<circle cx="%fcm" cy="%fcm" r="0.5cm" stroke="black" fill="none" stroke-width="1px"/>' % (x, y))


print('<svg width="5cm" height="5cm">')
if mode == front:
    printAlignmentHole(1, 4)
    printAlignmentHole(4, 4)
i = 0
for y in range(n):
    for x in range(n):
        cx = x + 0.5
        cy = y + 0.5
        if mode == numbers:
            printLabel(cx, cy, i + 1)
            printCutCircle(cx, cy)
        elif mode == front:
            printLabel(cx, cy, frontLabels[i])
        elif mode == back:
            printLabel(cx, cy, backLabels[i])
            printCutCircle(cx, cy)
        else:
            raise Exception("unknown mode")
        i += 1
print('</svg>')

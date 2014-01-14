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

# make an n x n grid of circles
if mode == numbers:
    n = 5
else:
    n = 4
fs = 0.6   # nominal font height in cm

def printLabel(x, y, label):
    print('<text x="%fcm" y="%fcm" text-anchor="middle" font-family="sans-serif" font-size="%fcm" fill="black">%s</text>' % (x, y + fs / 2.4, fs, label))

def printFrame():
    print('<rect x="0cm" y="0cm" width="%dcm" height="%dcm" fill="none" stroke="yellow" stroke-width="1px"/>' % (n, n))

def printCutCircle(x, y):
    print('<circle cx="%fcm" cy="%fcm" r="0.5cm" stroke="yellow" fill="none" stroke-width="1px"/>' % (x, y))


print('<svg width="%d.1cm" height="%d.1cm">' % (n, n))
if mode == front:
    printFrame()
i = 0
for y in range(n):
    for x in range(n):
        cx = x + 0.5
        cy = y + 0.5
        if mode == numbers:
            printLabel(cx, cy, i + 1)
            printCutCircle(cx, cy)
        elif mode == front:
            if x < n - 1 or y < n - 1:
                printLabel(cx, cy, 'O')
        elif mode == back:
            if x < n - 1 or y < n - 1:
                printLabel(cx, cy, 'N')
            printCutCircle(cx, cy)
        else:
            raise Exception("unknown mode")
        i += 1
print('</svg>')

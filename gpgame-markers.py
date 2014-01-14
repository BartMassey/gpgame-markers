#!/usr/bin/python3
# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.
# Generate SVG for laser-cut Glass Plate Game markers

from sys import argv

fs = 0.6     # nominal font height in cm
kerf = 0.2   # nominal kerf width in cm
pad = 0.1    # nominal doc edge pad width in cm

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

def printFrame(color):
    xy = pad
    wh = n * (1 + kerf) + kerf
    print('<rect x="%gcm" y="%gcm" width="%gcm" height="%gcm" fill="none" stroke="%s" stroke-width="1px"/>' % (xy, xy, wh, wh, color))

def printCutCircle(x, y):
    print('<circle cx="%fcm" cy="%fcm" r="0.5cm" stroke="yellow" fill="none" stroke-width="1px"/>' % (x, y))

d = n * (1 + kerf) + kerf + 2 * pad
print('<svg width="%gcm" height="%gcm">' % (d, d))
if mode == front:
    printFrame('yellow')
elif mode == back:
    printFrame('black')

i = 0
for y in range(n):
    for x in range(n):
        cx = x * (1 + kerf) + kerf + pad + 0.5
        cy = y * (1 + kerf) + kerf + pad + 0.5
        if mode == numbers:
            label = str(i + 1)
            if label == '6' or label == '9':
                label += '.'
            printLabel(cx, cy, label)
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

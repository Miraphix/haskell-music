module Tune where

semitoneRatio :: Float
semitoneRatio = 2 ** (1 / 12)

c4 = 261.6 :: Float
cs4 = c4 * semitoneRatio
d4 = cs4 * semitoneRatio
ds4 = d4 * semitoneRatio
e4 = ds4 * semitoneRatio
f4 = e4 * semitoneRatio
fs4 = f4 * semitoneRatio
g4 = fs4 * semitoneRatio
gs4 = g4 * semitoneRatio
a4 = gs4 * semitoneRatio
as4 = a4 * semitoneRatio
b4 = as4 * semitoneRatio

c3 = c4 / 2
d3 = d4 / 2
e3 = e4 / 2
f3 = f4 / 2
g3 = g4 / 2
a3 = a4 / 2
b3 = b4 / 2

c5 = c4 * 2
d5 = d4 * 2
e5 = e4 * 2
f5 = f4 * 2
g5 = g4 * 2
a5 = a4 * 2
b5 = b4 * 2

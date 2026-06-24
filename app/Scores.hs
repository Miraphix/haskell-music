module Scores where

import Data.Bifunctor (second)
import Timbre
import Tune
import Types

rawTruE :: Wave
rawTruE =
  foldl (<>) [] $
    map
      (uncurry (timbrePiano 0.2) . second (* quarter))
      [ ---
        (e5, 2),
        (d5, 1),
        (c5, 0.5),
        (b4, 0.5),
        ---
        (a4, 0.5),
        (g4, 0.25),
        (a4, 2.25),
        (g4, 0.5),
        (c5, 0.5),
        ---
        (e5, 0.5),
        (d5, 0.25),
        (e5, 1.75),
        (g4, 0.5),
        (f5, 1),
        ---
        (d5, 0.5),
        (c5, 0.25),
        (d5, 3),
        (0, 0)
      ]
  where
    quarter = 60 / 80 :: Float

rawBocchi :: Wave
rawBocchi =
  foldl (<>) [] $
    map
      (uncurry (timbrePiano 0.2) . second (* sixteenth))
      [ ---
        (c5, 1), (b4, 1), (a4, 1), (b4, 1), (0, 1), (g4, 1), (0, 1), (e4, 1),
        (0, 1), (c5, 1), (0, 1), (b4, 1), (0 ,2), (b4, 1), (0, 1),
        (c5, 1), (b4, 1), (a4, 1), (b4, 1), (0, 1), (g4, 1), (0, 1), (e4, 1),
        (0, 1), (c5, 1), (0, 1), (b4, 1), (0 ,2), (b4, 1), (b4, 1),
        (c5, 1), (b4, 1), (a4, 1), (b4, 1), (0, 1), (g4, 1), (0, 1), (e4, 1),
        (0, 1), (c5, 1), (0, 1), (b4, 1), (0 ,2), (b4, 1), (0, 1),
        (d5, 1), (0, 1), (e5, 1), (a4, 1), (0, 1), (c5, 1), (0, 1), (d5, 1),
        (0, 1), (e5, 1), (0, 1), (g5, 1), (0, 1), (e5, 1), (0, 2),
        (a5, 1), (0, 1), (g5, 1), (0, 1), (d5, 1), (e5, 1), (0, 1), (e5, 1),
        (0, 1), (e5, 1), (0, 1), (a4, 1), (0, 1), (g4, 1), (0, 1), (a4, 1),
        (0, 0)
      ]
  where
    sixteenth = 60 / 320 :: Float

rawNoise :: Wave
rawNoise = noise 0.1 5

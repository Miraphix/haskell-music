module Main where

import Data.Bifunctor (second)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as L
import Data.List (tails)
import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH.PprLib (semi)
import System.Process (runCommand)
import Tune as T

type Amplitude = Float

type Freq = Float

type Wave = [Float]

fp :: FilePath
fp = "output.bin"

asin2πω :: Amplitude -> Freq -> Float -> Float
asin2πω a f t = a * sin (2 * pi * f * t)

triangular :: Amplitude -> Freq -> Float -> Float
triangular a f t = 2 * a * g tn
  where
    tn = f * t - fromIntegral (floor $ f * t)
    g t = abs (2 * tn + 0.5) - abs (2 * tn - 0.5) + abs (2 * tn - 1.5) - 1.5

saw :: Amplitude -> Freq -> Float -> Float
saw a f t = 2 * a * g tn
  where
    tn = f * t - fromIntegral (floor $ f * t)
    g t
      | t < 0.5 = 2 * t
      | otherwise = 2 * t - 2

timbreSine :: Freq -> Float -> Wave
timbreSine freq duration = _wave freq
  where
    _wave freq = map (asin2πω 0.02 freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

timbreTriangular :: Freq -> Float -> Wave
timbreTriangular freq duration =
  _wave freq
    `mix` _wave (freq / semitoneRatio ** 12)
    `mix` _wave (freq * semitoneRatio ** 12)
  where
    _wave freq = map (triangular 0.1 freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

timbreSaw :: Freq -> Float -> Wave
timbreSaw freq duration =
  _wave freq
    `mix` _wave (freq * semitoneRatio ** 0.1)
    `mix` _wave (freq * semitoneRatio ** 0.2)
    `mix` _wave (freq / semitoneRatio ** 0.1)
    `mix` _wave (freq / semitoneRatio ** 0.2)
  where
    _wave freq = map (saw 0.1 freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

mix :: Wave -> Wave -> Wave
a `mix` b = zipWith (+) a b

rawTruE =
  foldl (<>) [] $
    map
      (uncurry timbreSaw . second (* quarter))
      [ ---
        (e5, 2),
        (d5, 1),
        (c5, 0.5),
        (b4, 0.5),
        ---
        (a4, 0.5),
        (g4, 0.25),
        (a4, 1.75),
        (0, 0.5),
        (g4, 0.5),
        (c5, 0.5),
        ---
        (e5, 0.5),
        (d5, 0.25),
        (e5, 1.25),
        (0, 0.5),
        (g4, 0.5),
        (f5, 1),
        ---
        (d5, 0.5),
        (c5, 0.25),
        (d5, 1.25),
        (0, 0.5)
      ]
  where
    quarter = 60 / 80 :: Float

smooth :: Int -> Wave -> Wave
smooth n xs = map f ws
  where
    f :: Wave -> Float
    f ws = sum ws / fromIntegral (length ws)
    windows n xs = map (take n) (tails xs)
    ws = windows n xs

save :: FilePath -> Wave -> IO ()
save fp raw =
  L.writeFile fp $
    B.toLazyByteString $
      foldMap B.floatLE $
        raw

main :: IO ()
main = do
  save fp rawTruE
  _ <- runCommand "ffplay -showmode 2 -f f32le -ar 48k output.bin &> /dev/null"
  return ()

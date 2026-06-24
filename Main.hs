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

timbreSine :: Amplitude -> Freq -> Float -> Wave
timbreSine a freq duration = _wave freq
  where
    _wave freq = map (asin2πω a freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

timbreTriangular :: Amplitude -> Freq -> Float -> Wave
timbreTriangular a freq duration =
  _wave freq
    `mix` _wave (freq / semitoneRatio ** 12)
    `mix` _wave (freq * semitoneRatio ** 12)
  where
    _wave freq = map (triangular a freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

timbreSaw :: Amplitude -> Freq -> Float -> Wave
timbreSaw a freq duration =
  mixUp 
    [ _wave 0.1 freq,
      _wave 0.05 (freq * semitoneRatio ** 12),
      _wave 0.01 (freq * semitoneRatio ** 24),
      _wave 0.05 (freq / semitoneRatio ** 12),
      _wave 0.01 (freq / semitoneRatio ** 24)
    ]
  where
    _wave _a freq = map (saw _a freq) xs
    xs = [0.0, step .. duration]
    step = 1 / 48000 :: Float

timbrePiano :: Freq -> Float -> Wave
timbrePiano = undefined

mix :: Wave -> Wave -> Wave
a `mix` b = zipWith (+) a b

mixUp :: [Wave] -> Wave
mixUp ws = foldr mix (head ws) (tail ws)

rawTruE =
  foldl (<>) [] $
    map
      (uncurry (timbreSaw 0.1) . second (* quarter))
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

harmonic :: Float -> Int -> Float
harmonic f0 n = nf * f0 * sqrt (1 + 1e-4 * nf * nf)
  where
    nf = fromIntegral n :: Float

attenuation :: Int -> Float -> Float
attenuation n t = exp $ -(t / taun)
  where
    taun = tau0 / (1 + alpha * fromIntegral n)
    tau0 = 10
    alpha = 1

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

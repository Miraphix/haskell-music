module Timbre where

import System.Random
import Global
import Tune
import Types
import Utils

asin2πω :: Amplitude -> Freq -> Float -> Float
asin2πω a f t = a * sin (2 * pi * f * t)

triangular :: Amplitude -> Freq -> Float -> Float
triangular a f t = 2 * a * ys
  where
    tn = f * t - fromIntegral (floor $ f * t :: Int)
    ys = abs (2 * tn + 0.5) - abs (2 * tn - 0.5) + abs (2 * tn - 1.5) - 1.5

saw :: Amplitude -> Freq -> Float -> Float
saw a f t = a * ys
  where
    tn = f * t - fromIntegral (floor $ f * t :: Int)
    ys
      | tn < 0.5  = 2 * tn
      | otherwise = 2 * tn - 2

timbreSine :: Amplitude -> Freq -> Float -> Wave
timbreSine a freq duration = _wave freq
  where
    _wave fr = map (asin2πω a fr) xs
    xs = [0.0, step .. duration]
    step = 1 / bitRate :: Float

timbreTriangular :: Amplitude -> Freq -> Float -> Wave
timbreTriangular a freq duration =
  _wave freq
    `mix` _wave (freq / semitoneRatio ** 12)
    `mix` _wave (freq * semitoneRatio ** 12)
  where
    _wave fr = map (triangular a fr) xs
    xs = [0.0, step .. duration]
    step = 1 / bitRate :: Float

timbreSaw :: Amplitude -> Freq -> Float -> Wave
timbreSaw a freq duration =
  map (*a) $
    mixUp
      [ _wave 0.1 freq,
        _wave 0.05 (freq * semitoneRatio ** 12),
        _wave 0.01 (freq * semitoneRatio ** 24),
        _wave 0.05 (freq / semitoneRatio ** 12),
        _wave 0.01 (freq / semitoneRatio ** 24)
      ]
  where
    _wave _a fr = map (saw _a fr) xs
    xs = [0.0, step .. duration]
    step = 1 / bitRate :: Float

timbrePiano :: Amplitude -> Freq -> Float -> Wave
timbrePiano a freq duration = ns `mix` mixUp ws
  where
    ws = do
      let ip = zip [0 .. prog] an
      (index, ak) <- ip
      let _freq = harmonic freq index
      let raw = map (\x -> attenuation index x * asin2πω (a*ak) _freq x) xs
      return raw
    an = [exp $ -(fromIntegral i / 1.5) | i <- [0::Int .. prog]] :: [Amplitude]
    prog = 10 :: Int
    xs = [0.0, step .. duration]
    step = 1 / bitRate
    ns = waveLPF (noise 0.1 5e-3) 4000

noise :: Amplitude -> Float -> Wave
noise a duration = fst $ foldl f ([], gen) xs
  where
    gen = mkStdGen $ round (duration * bitRate)
    f (acc, g) _ = (mrd:acc, g')
      where
        (rd, g') = random g
        mrd = a * (rd * 2 - 1)
    xs = [0.0, step .. duration]
    step = 1 / bitRate :: Float

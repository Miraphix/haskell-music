module Utils where

import Data.List (tails)
import Global
import Types

mix :: Wave -> Wave -> Wave
a `mix` b = zipWith (+) a b ++ drop (min la lb) (if la > lb then a else b)
  where
    la = length a
    lb = length b

mixUp :: [Wave] -> Wave
mixUp ws = foldr mix (head ws) (tail ws)

waveLPF :: Wave -> Freq -> Wave
waveLPF wave wc = fst $ foldr ff ([], y0) wave
  where
    ff uthis (acc, yprev) = (ythis : acc, ythis)
      where
        ythis = yk yprev uthis
    y0 = head wave
    yk ykm1 uk = (1 * ykm1 + _T * wc * uk) / (1 + _T * wc)
    _T = 1 / bitRate

smooth :: Int -> Wave -> Wave
smooth n xs = map f ws
  where
    f :: Wave -> Float
    f ws' = sum ws' / fromIntegral (length ws')
    windows n' xs' = map (take n') (tails xs')
    ws = windows n xs

harmonic :: Freq -> Int -> Freq
harmonic f0 n = nf * f0 * sqrt (1 + 1e-4 * nf * nf)
  where
    nf = fromIntegral n :: Float

attenuation :: Int -> Float -> Float
attenuation n t = exp $ -(t / taun)
  where
    taun = tau0 / (1 + alpha * fromIntegral n)
    tau0 = 1.2
    alpha = 2

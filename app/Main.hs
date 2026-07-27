module Main where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Complex
import Global
import Scores
import System.Process (runCommand)
import Types
import Utils
import Data.Word (Word32)

newtype WAVHead = WAVHead {
  len :: Word32
}

writeWAVHead :: WAVHead -> B.Builder
writeWAVHead wav = _riff <> _fmt <> _data
  where
    _riff = B.string7 "RIFF" <> (B.word32LE . len) wav <> B.string7 "WAVE"
    _fmt = B.string7 "fmt "
      <> B.word32LE 18 <> B.word16LE 3 <> B.word16LE 1 <> B.word32LE bitRateW
      <> B.word32LE (bitRateW*4) <> B.word16LE 4 <> B.word16LE 32 <> B.word16LE 0
    _data = B.string7 "data" <> (B.word32LE . (*4) . len) wav

save :: FilePath -> Wave -> IO ()
save filepath raw =
  L.writeFile filepath $
    B.toLazyByteString $
      writeWAVHead (WAVHead wlen) <> foldMap B.floatLE wav
  where
    wav = waveLPF raw 10000
    wlen = fromIntegral $ length wav :: Word32

dft :: [Double] -> [Complex Double]
dft xs = do
  index <- [0 .. _N - 1]
  let s = sum $ zipWith (*) xs' [ w _N (r*index) | r <- [0 .. _N-1]]
  return $ s / fromIntegral _N
  where
    _N = length xs
    xs' = map (:+ 0) xs
    w n r = cis $ -(2*pi* fromIntegral r / fromIntegral n)

main :: IO ()
main = do
  save fp rawTruE
  _ <- runCommand "ffplay -showmode 2 output.wav &> /dev/null"
  return ()

module Main where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Global
import Scores
import System.Process (runCommand)
import Types
import Utils

save :: FilePath -> Wave -> IO ()
save filepath raw =
  L.writeFile filepath $
    B.toLazyByteString $
      foldMap B.floatLE $
        waveLPF raw 4000

main :: IO ()
main = do
  save fp rawBocchi
  _ <- runCommand "ffplay -showmode 2 -f f32le -ar 48k output.bin &> /dev/null"
  return ()

module IO (readInput, writeOutput, someFunc) where

import Graphics.Image qualified as Image
import PPM qualified as P (PPM)
import QuadTree qualified as QT (QuadTree)

data IOFile
  = JPG String
  | PNG String

data ImageRep = PPM | QuadTree

readInput :: IOFile -> P.PPM
readInput = undefined

writeOutput :: ImageRep -> IO ()
writeOutput = undefined

someFunc :: String
someFunc = "Hello CIS 5520"

-- State for compression ratio
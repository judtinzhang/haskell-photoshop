module Main where

import Codec.Picture
import IO (readInput, toJpg, toPng, writeOutput)
import PPM
import QuadTree

main :: IO ()
main = do
  p <- readInput "crab.png"
  case p of
    Nothing -> print "error"
    Just ppm -> do
      -- let qt = qtRotateLeft $ compress ppm
      let qt = qtChangeColor yellow (100, 100, 255, 50) $ compress ppm
      -- let qt = qtRotateRight $ qtGrayscale $ qtReflectVertical $ compress ppm
      let image = writeOutput $ (decompress qt)
      -- let transformed = ppmChangeColor yellow (0, 0, 200, 255) ppm
      -- let transformed = ppmRotateRight ppm
      -- let image = writeOutput transformed
      -- let transformed = ppmCrop 100 500 200 900 ppm
      toPng (ImageRGBA8 image)

-- abstract out user interface
-- benchmarking
-- stack exec project-cis5520-exe
-- new type to wrap PPM / RGBA
-- Data.Vector

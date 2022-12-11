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
      -- let qt = qtChangeColor yellow (0, 0, 255, 255) $ compress ppm
      let qt = qtGrayscale $ qtReflectVertical $ compress ppm
      let image = writeOutput $ ppmBlur (decompress qt) 10
      -- let transformed = ppmChangeColor yellow (0, 0, 200, 255) ppm
      -- let transformed = ppmRotateRight ppm
      -- let image = writeOutput transformed
      -- let transformed = ppmCrop 100 500 200 900 ppm
      toPng (ImageRGBA8 image)
module Main where

import Codec.Picture
import IO (readInput, toJpg, toPng, writeOutput)
import PPM
import QuadTree (compress, decompress)

main :: IO ()
main = do
  p <- readInput "crab.png"
  case p of
    Nothing -> print "error"
    Just ppm ->
      let transformed = ppmRotateLeft ppm
       in let qt = compress transformed
           in let ppm1 = decompress qt
               in -- let transformed = ppmChangeColor yellow (0, 0, 200, 255) ppm in
                  -- let transformed = ppmCrop 100 500 200 900 ppm
                  do
                    -- let (r', g', b', a') = transformed
                    -- print ppm
                    -- print transformed
                    -- putStrLn
                    let image = writeOutput ppm1
                     in toPng (ImageRGBA8 image)
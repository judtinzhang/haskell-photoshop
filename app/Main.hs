module Main where

import Codec.Picture
import IO (readInput, toJpg, toPng, writeOutput)
import PPM

main :: IO ()
main = do
  p <- readInput "crab.png"
  case p of
    Nothing -> print "error"
    Just ppm -> 
      -- let transformed = ppmChangeColor yellow (0, 0, 200, 255) ppm in
      -- let transformed = ppmCrop 100 500 200 900 ppm 
      let transformed = ppmBlur ppm 3 in do 
      -- let (r', g', b', a') = transformed
      -- print ppm
      -- print transformed
      -- putStrLn 
      let image = writeOutput transformed in
       toPng (ImageRGBA8 image)

-- print ppm

-- let image = writeOutput [[(255, 0, 0, 255), (0, 255, 0, 255)], [(0, 0, 255, 255), (0, 0, 0, 255)]]
-- let image = writeOutput ppm
--   in --  in print $ imageHeight image

--     toPng (ImageRGBA8 image)

imageCreator = writePng "hi.png" $ generateImage pixelRenderer 250 250
  where
    pixelRenderer :: Int -> Int -> PixelRGB8
    pixelRenderer x y =
      let r = PixelRGB8 (fromIntegral x) (fromIntegral y) 128 
       in r
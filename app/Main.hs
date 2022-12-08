module Main where

-- import Codec.Picture

import Codec.Picture
import IO (readInput, toJpg, toPng, writeOutput)

-- import IO

main :: IO ()
main = do
  p <- readInput "car.png"
  case p of
    Nothing -> print "error"
    Just ppm ->
      let image = writeOutput ppm
       in toPng (ImageRGBA8 image)

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

-- PixelRGB8
-- (PixelYA8 x)
-- 22
-- 128

-- print p
-- case p of
-- Nothing -> print "error"
-- Just x -> print x

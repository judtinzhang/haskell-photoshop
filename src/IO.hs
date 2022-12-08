module IO (readInput, writeOutput, toJpg, toPng, doPrint) where

import PPM qualified as P (PPM)
import QuadTree qualified as QT (QuadTree)
import Codec.Picture
import qualified Codec.Picture.Types as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

convert :: String -> String -> IO ()
convert s t = do
  dImage <- readImage s
  case dImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 image) ->
      print "ImageRGB8"
    Right (ImageRGBA8 image) ->
      -- print "ImageRGBA8"
      print $ imageWidth image
      -- print $ VS.toList $ imageData image
      -- let ppm = vecToPPM (VS.toList $ imageData image) (imageWidth image) in
      -- print $ length $ head ppm
    Right _ -> putStrLn "Unexpected pixel format"

      -- let x = createPPM (VS.toList (imageData im)) (imageWidth im) in
      -- let (w, h, v) = createImage x in
      -- let z = ImageRGBA8 w h v in
      -- let y = toImage x 15 15 in
      -- saveJpgImage 2 t y
--  = undefined

-- 6, 13

toJpg :: DynamicImage -> IO ()
toJpg = saveJpgImage 100 "output.jpg"

toPng :: DynamicImage -> IO ()
toPng = savePngImage "output.png"

createPPMRow :: [a] -> Int -> Int -> ([(a, a, a, a)], [a])
createPPMRow stream@(r : g : b : a : xs) c w =
  if c < w
    then
      let (ret', remaining') = createPPMRow xs (c + 1) w in
      let remaining = remaining' in
      let ret = (r, g, b, a) : ret' in
      (ret, remaining)
    else ([], stream)
createPPMRow _ _ _ = ([], [])

vecToPPM :: [a] -> Int -> [[(a, a, a, a)]]
vecToPPM v@(r : g : b : a : xs) w = ret : vecToPPM remaining w
  where (ret, remaining) = createPPMRow v 0 w
vecToPPM _ _ = []



doPrint :: IO ()
doPrint = convert "dmnd.png" "crab.jpg"

-- car.jpg
-- crab.jpg
-- icon.jpg
-- icon1y

data IOFile
  = JPG String
  | PNG String

data ImageRep = PPM | QuadTree

-- readInput :: String -> Maybe P.PPM
readInput :: String -> IO (Maybe [[(Pixel8, Pixel8, Pixel8, Pixel8)]])
readInput i = do
  dImage <- readImage i
  case dImage of
    Left err -> return Nothing
    Right (ImageRGB8 image) -> return Nothing
    Right (ImageRGBA8 image) ->
      let ppm = vecToPPM (VS.toList $ imageData image) (imageWidth image) in
      return $ Just ppm
    Right _ -> return Nothing

writeOutput :: [[(Pixel8, Pixel8, Pixel8, Pixel8)]] -> Image PixelRGBA8
writeOutput i =
  generateImage gen (length (head i)) (length i)
  where
    gen x y =
      let (r, g, b, a) = i !! y !! x in
      PixelRGBA8 r g b a

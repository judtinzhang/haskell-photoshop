module IO (readInput, writeOutput, toJpg, toPng, doPrint) where

import PPM qualified as P (PPM, RGBA)
import QuadTree qualified as QT (QuadTree)
import Codec.Picture
import qualified Codec.Picture.Types as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Debug.Trace

-- ================= DEBUGGING =========================

debugger :: String -> String -> IO ()
debugger s t = do
  dImage <- readImage s
  case dImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 image) ->
      -- print $ length $ rgbToRGBA $ VS.toList $ imageData image
      -- print $ length $ VS.toList $ imageData image
      print "ImageRGBA8"
    Right (ImageRGBA8 image) ->
      -- print "ImageRGBA8"
      -- print $ imageWidth image
      -- print $ length $ rgbToRGBA $ VS.toList $ imageData image
      print $ VS.toList $ imageData image
      -- let ppm = vecToPPM (VS.toList $ imageData image) (imageWidth image) in
      -- print $ length $ head ppm
    Right (ImageRGBF image) -> print 4
    Right (ImageYCbCr8 image) -> print 5
      -- print (length $ ycbToRGBA $ VS.toList $ imageData image)
      -- let ppm = vecToPPM (VS.toList $ imageData image) (imageWidth image) in
      -- print ppm
    Right _ -> putStrLn "Unexpected pixel format"

doPrint :: IO ()
doPrint = debugger "crab.png" "crab.jpg"

-- =====================================================

toDouble :: Pixel8 -> Double
toDouble = fromIntegral . toInteger

toPixel8 :: Double -> Pixel8
toPixel8 = fromIntegral . round

rgbToRGBA :: [Pixel8] -> [Pixel8]
rgbToRGBA (r : g : b : xs) = r : g : b : 255 : rgbToRGBA xs
rgbToRGBA xs = xs

ycbcrToRGBA :: [Pixel8] -> [Pixel8]
ycbcrToRGBA (y : cb : cr : xs) = fromIntegral r : fromIntegral g : fromIntegral b : 255 : ycbcrToRGBA xs
  where
    y' :: Double
    y' = fromIntegral $ toInteger y - 16
    cb' :: Double
    cb' = fromIntegral (toInteger cb) - 128
    cr' :: Double
    cr' = fromIntegral (toInteger cr) - 128
    r = round $ y' + cr' * 1.402 :: Int
    g = round $ y' + cb' * (-0.344136) + cr' * (-0.714136) :: Int
    b = round $ y' + cb' * 1.772 :: Int
ycbcrToRGBA xs = xs

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

-- readInput :: String -> IO (Maybe [[(Pixel8, Pixel8, Pixel8, Pixel8)]])
readInput :: String -> IO (Maybe P.PPM)
readInput i = do
  dImage <- readImage i
  case dImage of
    Left err -> return Nothing
    Right (ImageRGB8 image) ->
      let ppm = vecToPPM (map toDouble (rgbToRGBA $ VS.toList $ imageData image)) (imageWidth image) in
      return $ Just ppm
    Right (ImageRGBA8 image) ->
      let ppm = vecToPPM (map toDouble (VS.toList $ imageData image)) (imageWidth image) in
      return $ Just ppm
    Right (ImageYCbCr8 image) ->
      let ppm = vecToPPM (map toDouble (ycbcrToRGBA $ VS.toList $ imageData image)) (imageWidth image) in
      return $ Just ppm
    Right _ -> return Nothing

writeOutput :: P.PPM -> Image PixelRGBA8
writeOutput i =
  generateImage gen (length (head i)) (length i)
  where
    gen x y =
      let (r, g, b, a) = i !! y !! x in
      PixelRGBA8 (toPixel8 r) (toPixel8 g) (toPixel8 b) (toPixel8 a)
-- writeOutput (QtRep i) = undefined

-- TODO: still use?

data IOFile
  = JPG String
  | PNG String

-- data ImageRep 
--   = PpmRep P.PPM
--   | QtRep (QT.QuadTree P.RGBA)


module IO (readInput, writeOutput, someFunc, toJpg, carToJpg, getPpm) where

-- import Graphics.Image qualified as I
-- import Graphics.Image.Processing (rotate180)

-- import qualified Graphics.Image.Interface.Repa as R

import PPM qualified as P (PPM)
import QuadTree qualified as QT (QuadTree)

-- import Prelude (IO, String, (+), Maybe, Either(..))
import Codec.Picture
import qualified Codec.Picture.Types as M
-- import qualified GHC.Num as GHN
--import Data.List (toList)
-- import Data.Foldable (toList)
-- import Data.List.Split


--import JuicyPixels

toJpg :: String -> String -> IO ()
toJpg s t = do
  img <- readImage s
  case img of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right im -> saveJpgImage 2 t im

carToJpg :: IO ()
carToJpg = toJpg "car.png" "car.jpg"


getPpmRow :: Image PixelRGB8 -> Int -> Int -> [PixelRGB8]
getPpmRow i x y = case compare x (imageWidth i) of 
  lt -> pixelAt i x y : readRow i (x + 1) y
  -- _ -> []

getPpmHelper :: Image PixelRGB8 -> Int -> [[PixelRGB8]]
getPpmHelper i y = case compare y (imageHeight i) of 
  lt -> readRow i 0 y : readImgHelper i (y + 1)
  -- _ -> []

getPpm :: Image PixelRGB8 -> [[PixelRGB8]]
readImg i = readImgHelper i 0

getPpm :: IO (Maybe [[PixelRGB8]])
carGetPpm = do
  img <- readImage "car.png"
  case img of
    Right (ImageRGB8 im) -> return $ Just $ readImg im
    _ -> undefined

data IOFile
  = JPG String
  | PNG String

data ImageRep = PPM | QuadTree

readInput :: IOFile -> Maybe ImageRep
readInput = undefined

writeOutput :: ImageRep -> IO ()
writeOutput = undefined

someFunc :: String
someFunc = "Hello CIS 5520"

-- State for compression ratio
-- criterion package for benchmarking


-- convertToPixels :: DynamicImage -> ([Pixel8], Int, Int)
-- convertToPixels = M.extractRGB8 

-- convertToArray :: Image PixelRGB8 -> [[(Pixel8, Pixel8, Pixel8)]]
-- convertToArray img = chunksOf (imageWidth img) (toList img)

-- -- Convert a dynamic image to a 2D matrix of pixels
-- convertToMatrix :: DynamicImage -> [[(Pixel8, Pixel8, Pixel8)]]
-- convertToMatrix img = do
--   let (pixels, width, _) = extractRGB8 img
--   chunksOf width pixels


-- convertToMatrix :: DynamicImage -> [[(Pixel8, Pixel8, Pixel8)]]
-- convertToMatrix :: DynamicImage -> [[PixelRGB8]]
--convertToMatrix img = do
 -- let imgRGB8 = convertRGB8 img
  --let pixels = toList imgRGB8
  --chunksOf (imageWidth imgRGB8) pixels


      -- let y = convertToArray p in

      -- let p = convertToArray t in

      -- print $ convertToArray p
      -- print $ toList p
      --print (pixelMap id p)

     -- let pixels = convertToPixels t in 
      --print pixels
      --saveJpgImage 2 "hi.jpg" t


-- let pixels = convertToPixels img

--   -- Print the pixel data, image width, and image height
--   print pixels



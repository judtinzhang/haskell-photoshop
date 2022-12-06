module IO (readInput, writeOutput, someFunc, toJpg, carToJpg) where

-- import Graphics.Image qualified as I
-- import Graphics.Image.Processing (rotate180)
-- import qualified Graphics.Image.Interface.Repa as R
-- import Prelude (IO, String, (+), Maybe, Either(..))
-- import Codec.Picture.R
-- import qualified GHC.Num as GHN
-- import Data.List (toList)
-- import Data.Foldable (toList)
-- import Data.List.Split
-- import Data.List qualified as List
-- import JuicyPixels

import PPM qualified as P (PPM)
import QuadTree qualified as QT (QuadTree)
import Codec.Picture
import qualified Codec.Picture.Types as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

toJpg :: String -> String -> IO ()
toJpg s t = do
  img <- readImage s
  case img of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right im@(ImageRGBA8 d) ->
      let x = createPPM (VS.toList (imageData d)) (imageWidth d) in
      print x
      -- saveJpgImage 2 t im
    _ -> putStrLn "Error"

createPPMRow :: [Pixel8] -> Int -> Int -> ([(Pixel8, Pixel8, Pixel8, Pixel8)], [Pixel8])
createPPMRow (r : g : b : a : xs) c w =
  if c < w
    then
      let (ret', remaining') = createPPMRow xs (c + 1) w in
      let remaining = remaining' in
      let ret = (r, g, b, a) : ret' in
      (ret, remaining)
    else ([], xs)
createPPMRow _ _ _ = ([], [])

createPPM :: [Pixel8] -> Int -> [[(Pixel8, Pixel8, Pixel8, Pixel8)]]
createPPM v@(r : g : b : a : xs) w = ret : createPPM remaining w
  where (ret, remaining) = createPPMRow v 0 w
createPPM _ _ = []

carToJpg :: IO ()
carToJpg = toJpg "icon.png" "icon.jpg"


























-- pixelBaseComponentToPixelRGBA8 :: M.PixelBaseComponent PixelRGBA8 -> PixelRGBA8
-- pixelBaseComponentToPixelRGBA8 (M.PixelBaseComponent r g b a) = PixelRGBA8 r g b a

-- processVector :: VS.Vector Pixel8 -> [[(Pixel8, Pixel8, Pixel8, Pixel8)]]
-- processVector (x : xs) =



-- = VS.foldr(\x acc -> acc + 1) 0
  -- where
  --   ll :: Pixel8 -> Int
  --   ll x = 1
-- processVector = VS.foldr (\x acc -> 1 + acc) 0

-- processVector :: VS.Vector PixelRGBA8 -> Int
-- processVector = VS.foldr (\x acc -> 1 + acc) 0
-- pp ::
-- pp =

-- processVector :: VS.Vector (PixelBaseComponent PixelRGBA8) -> IO ()
-- processVector =  VS.foldr (\x acc -> ll x) (putStrLn "hi")








  -- where
  -- ll :: PixelBaseComponent PixelRGBA8  -> IO ()
  -- ll (PixelRGBA8 a s d f) = putStrLn "hi"
    -- let x :: (PixelRGBA8) = toPixelRGBA8 p in
    -- putStrLn ("hi")
    -- ll :: PixelBaseComponent PixelRGBA8 -> IO ()
    -- -- ll _ = putStrLn (show 1)
    -- ll (PixelRGBA8 a b c d) = putStrLn (show 1)




    -- PixelBaseComponent  = putStrLn "hello"

-- processVector = VS.foldr (\x acc -> 1 + acc) 0


  -- putStrLn (show (VS.length v))


  -- VS.foldr (\x acc -> helper x) (putStrLn "")
  -- where
  --   helper :: PixelBaseComponent PixelRGBA8 -> IO ()
  --   helper x = putStrLn "hi + 2"

-- processVector = print








-- ImageRGB8 (Image PixelRGB8)
-- ImageRGB16 (Image PixelRGB16)
-- ImageRGBF (Image PixelRGBF)
-- ImageRGBA8 (Image PixelRGBA8)
-- ImageRGBA16 (Image PixelRGBA16)
-- ImageYCbCr8 (Image PixelYCbCr8)
-- ImageCMYK8 (Image PixelCMYK8)
-- ImageCMYK16 (Image PixelCMYK16)






-- getPpmRow :: Image PixelRGB8 -> Int -> Int -> [PixelRGB8]
-- getPpmRow i x y = case compare x (imageWidth i) of
--   lt -> pixelAt i x y : readRow i (x + 1) y
--   -- _ -> []

-- getPpmHelper :: Image PixelRGB8 -> Int -> [[PixelRGB8]]
-- getPpmHelper i y = case compare y (imageHeight i) of
--   lt -> readRow i 0 y : readImgHelper i (y + 1)
--   -- _ -> []

-- getPpm :: Image PixelRGB8 -> [[PixelRGB8]]
-- readImg i = readImgHelper i 0

-- getPpm :: IO (Maybe [[PixelRGB8]])
-- carGetPpm = do
--   img <- readImage "car.png"
--   case img of
--     Right (ImageRGB8 im) -> return $ Just $ readImg im
--     _ -> undefined

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



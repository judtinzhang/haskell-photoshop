module PPM
  ( PPM (..),
    rotateLeft,
    rotateRight,
    reflectHorizontal,
    reflectVertical,
    changeColor,
    saturate,
    grayscale,
    blur,
    crop,
    readInput,
    toJpg,
    toPng,
  )
where

import Codec.Picture
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Pixel

-- ================= IO ===========================

readInput :: String -> IO PPM
readInput i = do
  dImage <- readImage i
  case dImage of
    Left err -> error "Error: Cannot process image"
    Right (ImageRGB8 image) ->
      let ppm = vecToPPM (map toDouble (rgbToRGBA $ VS.toList $ imageData image)) (imageWidth image)
       in return ppm
    Right (ImageRGBA8 image) ->
      let ppm = vecToPPM (map toDouble (VS.toList $ imageData image)) (imageWidth image)
       in return ppm
    Right (ImageYCbCr8 image) ->
      let ppm = vecToPPM (map toDouble (ycbcrToRGBA $ VS.toList $ imageData image)) (imageWidth image)
       in return ppm
    Right _ -> error "Error: Cannot process image"

createPPMRow :: [a] -> Int -> Int -> ([(a, a, a, a)], [a])
createPPMRow stream@(r : g : b : a : xs) c w =
  if c < w
    then
      let (ret', remaining') = createPPMRow xs (c + 1) w
       in let remaining = remaining'
           in let ret = (r, g, b, a) : ret'
               in (ret, remaining)
    else ([], stream)
createPPMRow _ _ _ = ([], [])

vecToPPM :: [a] -> Int -> [[(a, a, a, a)]]
vecToPPM v@(r : g : b : a : xs) w = ret : vecToPPM remaining w
  where
    (ret, remaining) = createPPMRow v 0 w
vecToPPM _ _ = []

convertToImage :: PPM -> Image PixelRGBA8
convertToImage i =
  generateImage gen (length (head i)) (length i)
  where
    gen x y =
      let (r, g, b, a) = i !! y !! x
       in PixelRGBA8 (toPixel8 r) (toPixel8 g) (toPixel8 b) (toPixel8 a)

toJpg :: PPM -> String -> IO ()
toJpg p out_file =
  let image = convertToImage p
   in saveJpgImage 100 out_file (ImageRGBA8 image)

toPng :: PPM -> String -> IO ()
toPng p out_file =
  let image = convertToImage p
   in savePngImage out_file (ImageRGBA8 image)

-- ================= Image Processing ===========================

type PPM =
  [[RGBA]]

transposeList :: [[a]] -> [[a]]
transposeList [] = []
transposeList ([] : xss) = []
transposeList matrix =
  let (col, restCol) = transposeHelper matrix
   in col : transposeList restCol
  where
    transposeHelper :: [[a]] -> ([a], [[a]])
    transposeHelper [] = ([], [])
    transposeHelper ([] : _) = ([], [])
    transposeHelper ((x : xs) : xss) =
      let (row, restRow) = transposeHelper xss
       in (x : row, xs : restRow)

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

rotateLeft :: PPM -> PPM
rotateLeft = reverseList . transposeList

rotateRight :: PPM -> PPM
rotateRight = transposeList . reverseList

reflectHorizontal :: PPM -> PPM
reflectHorizontal = map reverseList

reflectVertical :: PPM -> PPM
reflectVertical = reverseList

changeColor :: RGBARange -> RGBA -> PPM -> PPM
changeColor cr t = map $ map $ pixelChangeColor cr t

saturate :: Double -> PPM -> PPM
saturate deltaS = map $ map $ pixelSaturate deltaS

grayscale :: PPM -> PPM
grayscale = map $ map pixelGrayScale

validIndex :: PPM -> Int -> Int -> Bool
validIndex ppm@(x : xs) i j = i >= 0 && i < length ppm && j >= 0 && j < length x
validIndex _ _ _ = False

updateAvg :: PPM -> Int -> Int -> (RGBA, Double) -> (RGBA, Double)
updateAvg ppm i j t@((r, g, b, a), count) =
  if validIndex ppm i j
    then
      let (r', g', b', a') = ppm !! i !! j
       in ((r + r', g + g', b + b', a + a'), count + 1)
    else t

neighborList :: Int -> Int -> Int -> [(Int, Int)]
neighborList i j radius = neighborListHelper i j radius (i - radius) (j - radius)

neighborListHelper :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
neighborListHelper i j radius curri currj =
  if curri == i + radius
    then
      if currj == j + radius
        then []
        else (curri, currj) : neighborListHelper i j radius (i - radius) (currj + 1)
    else (curri, currj) : neighborListHelper i j radius (curri + 1) currj

neighborAvg :: Int -> Int -> Int -> PPM -> RGBA
neighborAvg i j radius ppm =
  let nl = neighborList i j radius
   in let (p, count) = foldr (\(i, j) acc -> updateAvg ppm i j acc) ((0, 0, 0, 0), 0) nl
       in mapRGBA (/ count) p

blur :: PPM -> Int -> PPM
blur ppm = blurHelper ppm 0

blurHelper :: PPM -> Int -> Int -> PPM
blurHelper ppm v radius =
  if v < length ppm
    then blurRow ppm v 0 radius : blurHelper ppm (v + 1) radius
    else []

blurRow :: PPM -> Int -> Int -> Int -> [RGBA]
blurRow ppm i j radius =
  if j < length (ppm !! i)
    then neighborAvg i j radius ppm : blurRow ppm i (j + 1) radius
    else []

getSubArray :: Int -> Int -> [a] -> [a]
getSubArray start end array = take (end - start + 1) (drop start array)

-- startRow, endRow, startCol, endCol
crop :: Int -> Int -> Int -> Int -> PPM -> PPM
crop r1 r2 c1 c2 ppm =
  let rowCrop = getSubArray r1 r2 ppm
   in let colCrop = map (getSubArray c1 c2) rowCrop
       in colCrop
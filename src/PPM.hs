module PPM
  ( RGBA (..),
    PPM (..),
    ppmRotateLeft,
    ppmRotateRight,
    ppmReflectHorizontal,
    ppmReflectVertical,
    ppmChangeColor,
    ppmSaturate,
    ppmGrayscale,
    ppmBlur,
    ppmCrop,
    RGBARange,
    yellow,
  )
where

import Codec.Picture
import Pixel

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

ppmRotateLeft :: PPM -> PPM
ppmRotateLeft = reverseList . transposeList

ppmRotateRight :: PPM -> PPM
ppmRotateRight = transposeList . reverseList

ppmReflectHorizontal :: PPM -> PPM
ppmReflectHorizontal = map reverseList

ppmReflectVertical :: PPM -> PPM
ppmReflectVertical = reverseList

ppmChangeColor :: RGBARange -> RGBA -> PPM -> PPM
ppmChangeColor cr t = map $ map $ pixelChangeColor cr t

ppmSaturate :: Double -> PPM -> PPM
ppmSaturate deltaS = map $ map $ pixelSaturate deltaS

ppmGrayscale :: PPM -> PPM
ppmGrayscale = map $ map pixelGrayScale

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

ppmBlur :: PPM -> Int -> PPM
ppmBlur ppm = ppmBlurHelper ppm 0

ppmBlurHelper :: PPM -> Int -> Int -> PPM
ppmBlurHelper ppm v radius =
  if v < length ppm
    then ppmBlurRow ppm v 0 radius : ppmBlurHelper ppm (v + 1) radius
    else []

ppmBlurRow :: PPM -> Int -> Int -> Int -> [RGBA]
ppmBlurRow ppm i j radius =
  if j < length (ppm !! i)
    then neighborAvg i j radius ppm : ppmBlurRow ppm i (j + 1) radius
    else []

getSubArray :: Int -> Int -> [a] -> [a]
getSubArray start end array = take (end - start + 1) (drop start array)

-- startRow, endRow, startCol, endCol
ppmCrop :: Int -> Int -> Int -> Int -> PPM -> PPM
ppmCrop r1 r2 c1 c2 ppm =
  let rowCrop = getSubArray r1 r2 ppm
   in let colCrop = map (getSubArray c1 c2) rowCrop
       in colCrop
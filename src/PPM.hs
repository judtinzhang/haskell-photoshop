module PPM
  ( RGBA (..),
    -- Pixel,
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
import GHC.Float as FL
import Debug.Trace

-- data RGBA = (,,) Int Int Int deriving (Eq)
type RGBA = (Double, Double, Double, Double)
type HSL = (Double, Double, Double, Double)

mapRGBA :: (Double -> a) -> RGBA -> (a, a, a, a)
mapRGBA f c = (f r, f g, f b, f a) 
  where 
    (r, g, b, a) = c

data RGBARange = RGBARange {
  redRange :: (Double, Double),
  greenRange :: (Double, Double),
  blueRange :: (Double, Double),
  alphaRange :: (Double, Double)
}

yellow :: RGBARange 
yellow = RGBARange {
  redRange = (200, 255), 
  greenRange = (200, 255), 
  blueRange = (0, 255), 
  alphaRange = (0, 255)
}

type PPM =
  [[RGBA]]

transposeList :: [[a]] -> [[a]]
transposeList [] = []
transposeList ([] : xss )= []
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

pixelChangeColor :: RGBARange -> RGBA -> RGBA -> RGBA
pixelChangeColor cr t c = 
  if rlow <= r && r <= rhigh &&
     glow <= g && g <= ghigh &&
     blow <= b && b <= bhigh &&
     alow <= a && a <= ahigh
     then c
     else t
  where 
    (rlow, rhigh) = redRange cr
    (glow, ghigh) = greenRange cr
    (blow, bhigh) = blueRange cr
    (alow, ahigh) = alphaRange cr
    (r, g, b, a) = c

ppmChangeColor :: RGBARange -> RGBA -> PPM -> PPM
ppmChangeColor cr t = map $ map $ pixelChangeColor cr t

-- Converts a color from RGBA to HSL
rgbToHsl :: RGBA -> HSL
rgbToHsl (r, g, b, a) = (h, s, l, a)
  where
    r' = r / 255
    g' = g / 255
    b' = b / 255
    cmax = maximum [r', g', b']
    cmin = minimum [r', g', b']
    delta = cmax - cmin
    l = (cmax + cmin) / 2

    h 
      | cmax == r' = if delta == 0 then 0 else 60 * (((g' - b') / delta) `mod'` 6)
      | cmax == g' = if delta == 0 then 0 else 60 * (((b' - r') / delta) + 2) 
      | cmax == b' = if delta == 0 then 0 else 60 * (((r' - g') / delta) + 4)
      | otherwise = 0

    s = if l == 0 || l == 1 then 0 else delta / (1 - abs (2 * l - 1))

-- Converts a color from HSL to RGB
hslToRgb :: HSL -> RGBA
hslToRgb (h, s, l, a) = (r', g', b', a')
  where
    c = (1 - abs (2 * l - 1)) * s
    x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
    m = l - c / 2
    (r, g, b) = case h of
        h | h >= 0 && h < 60  -> (c, x, 0)
          | h >= 60 && h < 120 -> (x, c, 0)
          | h >= 120 && h < 180 -> (0, c, x)
          | h >= 180 && h < 240 -> (0, x, c)
          | h >= 240 && h < 300 -> (x, 0, c)
          | h >= 300 && h <= 360 -> (c, 0, x)
          | otherwise -> (0, 0, 0)
    r' = (r + m) * 255
    g' = (g + m) * 255
    b' = (b + m) * 255
    a' = traceShow ("rgb'", r', g', b') a

-- Gets positive modulo
mod' :: Double -> Double -> Double
mod' x y = 
  let m_x = double2Int x :: Int in 
  let m_y = double2Int y :: Int in
  int2Double ((m_x `mod` m_y + m_y) `mod` m_y)

pixelSaturate :: Double -> RGBA -> RGBA
pixelSaturate deltaS p = p'
  where 
    (h, s, l, a) = rgbToHsl p
    s' = max (min 1 deltaS) 0 -- keep saturation between 0 and 1
    p' = hslToRgb (h, s', l, a)

ppmSaturate :: Double -> PPM -> PPM
ppmSaturate deltaS = map $ map $ pixelSaturate deltaS

pixelGrayScale :: RGBA -> RGBA
pixelGrayScale (r, g, b, a) = (gray, gray, gray, a)
  where gray = r / 3 + g / 3 + b / 3

ppmGrayscale :: PPM -> PPM
ppmGrayscale = map $ map pixelGrayScale

validIndex :: PPM -> Int -> Int -> Bool
validIndex ppm@(x : xs) i j = i >= 0 && i < length ppm && j >= 0 && j < length x
validIndex _ _ _ = False

updateAvg :: PPM -> Int -> Int -> (RGBA, Double) -> (RGBA, Double)
updateAvg ppm i j t@((r, g, b, a), count) = if validIndex ppm i j then 
  let (r', g', b', a') = ppm !! i !! j in 
  ((r + r', g + g', b + b', a + a'), count + 1)
  else t

neighborList :: Int -> Int -> Int -> [(Int, Int)]
neighborList i j radius = neighborListHelper i j radius (i-radius) (j-radius)

neighborListHelper :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
neighborListHelper i j radius curri currj = 
  if curri == i + radius
    then if currj == j + radius
      then []
      else (curri, currj) : neighborListHelper i j radius (i - radius) (currj + 1)
    else (curri, currj) : neighborListHelper i j radius (curri + 1) currj

neighborAvg :: Int -> Int -> Int -> PPM -> RGBA
neighborAvg i j radius ppm = 
  let nl = neighborList i j radius in
  let (p, count) = foldr (\(i, j) acc -> updateAvg ppm i j acc) ((0, 0, 0, 0), 0) nl in
  mapRGBA (/count) p

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
  let rowCrop = getSubArray r1 r2 ppm in
  let colCrop = map (getSubArray c1 c2) rowCrop in
  colCrop
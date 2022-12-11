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
type RGBA = (Pixel8, Pixel8, Pixel8, Pixel8)
type HSL = (Pixel8, Pixel8, Pixel8, Pixel8)

mapRGBA :: (Pixel8 -> a) -> RGBA -> (a, a, a, a)
mapRGBA f c = (f r, f g, f b, f a) 
  where 
    (r, g, b, a) = c

data RGBARange = RGBARange {
  redRange :: (Int, Int),
  greenRange :: (Int, Int),
  blueRange :: (Int, Int),
  alphaRange :: (Int, Int)
}

yellow :: RGBARange 
yellow = RGBARange {
redRange = (200, 255), 
greenRange = (200, 255), 
blueRange = (0, 255), 
alphaRange = (0, 255)
}

-- newtype Pixel = P RGBA
--   deriving (Eq)

type PPM =
  [[RGBA]]

toDouble :: Pixel8 -> Double
toDouble = fromIntegral . toInteger

toDouble2 :: Pixel8 -> Int
toDouble2 = fromIntegral . toInteger

toPixel8 :: Double -> Pixel8
toPixel8 = fromIntegral . toInteger

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
    (r, g, b, a) = mapRGBA fromIntegral c

ppmChangeColor :: RGBARange -> RGBA -> PPM -> PPM
ppmChangeColor cr t = map $ map $ pixelChangeColor cr t

-- Converts a color from RGBA to HSL
rgbToHsl :: RGBA -> HSL
rgbToHsl (r, g, b, a) = (h, s, l, a')
    where
        r' :: Double
        r' = traceShow ("original", r, g, b) toDouble r / 255 
        g' :: Double
        g' = toDouble g / 255 
        b' :: Double
        b' = toDouble b / 255
        cmax = maximum [r', g', b']
        cmin = minimum [r', g', b']
        delta = traceShow ("cmax, cmin", cmax, cmin) cmax - cmin
        l = toPixel8 $ (cmax + cmin) / 2

        h = traceShow ("primes", r', g', b') (toPixel8 (if cmax == r' then if delta == 0 then 0 else 60 * FL.int2Double (FL.double2Int ((g' - b') / delta) `mod'` 6)
                   else if cmax == g' then if delta == 0 then 0 else 60 * (((b' - r') / delta) + 2) 
                   else if cmax == b' then if delta == 0 then 0 else 60 * (((r' - g') / delta) + 4)
                   else 0))
        
        
        --case cmax of
          --  r' -> if delta == 0 then 0 else 60 * FL.int2Double (FL.double2Int ((g' - b') / delta) `mod'` 6)
            --g' -> if delta == 0 then 0 else 60 * (((b' - r') / delta) + 2)
            --b' -> if delta == 0 then 0 else 60 * (((r' - g') / delta) + 4)
            --_  -> 0

        s = toPixel8 $ if l == 0 || l == 1 then 0 else delta / (1 - abs (2 * toDouble l - 1))
        a' = traceShow ("h, s, l", h, s, l) a


-- Converts a color from HSL to RGB
hslToRgb :: HSL -> RGBA
hslToRgb (h', s', l', a) = (r', g', b', a)
    where
        h :: Double
        h = toDouble h'
        s :: Double
        s = toDouble s'
        l :: Double
        l = toDouble l'
        c :: Double
        c = (1 - abs (2 * l - 1)) * s
        x :: Double
        x = c * (1 - abs (FL.int2Double (FL.double2Int (h / 60) `mod'` 2 - 1)))
        -- x = (h / 60) `mod` 2
        -- x = c * (1 - abs ((toDouble2fromIntegral h' / 60) `mod'` 2 - 1))

        --x = c * fromIntegral (1 - abs ((fromInteger (toInteger (h / 60) :: Integer) :: Int) `mod'` 2 - 1 :: Int))
        m :: Double
        m = l - c / 2
        (r, g, b) = case h of
            h | h >= 0 && h < 60  -> (c, x, 0)
              | h >= 60 && h < 120 -> (x, c, 0)
              | h >= 120 && h < 180 -> (0, c, x)
              | h >= 180 && h < 240 -> (0, x, c)
              | h >= 240 && h < 300 -> (x, 0, c)
              | h >= 300 && h <= 360 -> (c, 0, x)
              | otherwise -> (0, 0, 0)
        r' = toPixel8 $ (r + m) * 255
        g' = toPixel8 $ (g + m) * 255
        b' = toPixel8 $ (b + m) * 255

-- Gets positive modulo
mod' :: Integral a => a -> a -> a
mod' x y = (x `mod` y + y) `mod` y

pixelSaturate :: Double -> RGBA -> RGBA
pixelSaturate deltaS p = p'
  where 
    (h, s, l, a) = rgbToHsl p
    s' = toPixel8 $ max (min 1 (toDouble s+deltaS)) 0 -- keep saturation between 0 and 1
    p' = hslToRgb (h, s', l, a)

ppmSaturate :: Double -> PPM -> PPM
ppmSaturate deltaS = map $ map $ pixelSaturate deltaS

pixelGrayScale :: RGBA -> RGBA
pixelGrayScale (r, g, b, a) = (gray, gray, gray, a)
  where gray = toPixel8 $ toDouble r / 3 + toDouble g / 3 + toDouble b / 3

ppmGrayscale :: PPM -> PPM
ppmGrayscale = map $ map pixelGrayScale


-- doubleForLoopWithIndices :: (Int -> Int -> a -> b) -> [[a]] -> [b]
-- doubleForLoopWithIndices f matrix = concat [map (f rowIndex colIndex) row | (rowIndex, row) <- zip [0..] matrix]
-- doubleForLoopWithIndices :: (Int -> Int -> a -> b) -> [[a]] -> [b]
-- doubleForLoopWithIndices f matrix = concat [map (\(colIndex, x) -> f rowIndex colIndex x) (zip [0..] row) | (rowIndex, row) <- zip [0..] matrix]

validIndex :: PPM -> Int -> Int -> Bool
validIndex ppm@(x : xs) i j = i >= 0 && i < length ppm && j >= 0 && j < length x

updateAvg :: PPM -> Int -> Int -> (RGBA, Int) -> (RGBA, Int)
updateAvg ppm i j t@((r, g, b, a), count) = if validIndex ppm i j then 
  let (r', g', b', a') = ppm !! i !! j in 
  ((r + r', g + g', b + b', a + a'), count + 1)
  else t

neighborAvg :: Int -> Int -> PPM -> RGBA
neighborAvg i j ppm = 
  let avg0 = (0, 0) in
  let avg1 = updateAvg ppm i + 1 j avg0 in
  let avg2 = updateAvg ppm i + 1 j + 1 avg1 in
  let avg3 = updateAvg ppm i + 1 j - 1 avg2 in
  let avg4 = updateAvg ppm i - 1 j avg3 in
  let avg5 = updateAvg ppm i - 1 j + 1 avg4 in
  let avg6 = updateAvg ppm i - 1 j - 1 avg5 in
  let avg7 = updateAvg ppm i j avg6 in
  let avg8 = updateAvg ppm i j + 1 avg7 in
  let ((r, g, b, a), count9) = updateAvg ppm i j - 1 avg8 in
  (r / count9, g / count9, b / count9, a / count9)


-- ppmBlur :: PPM -> Int -> PPM
-- ppmBlur ppm i = doubleForLoopWithIndices 

ppmBlur :: PPM -> Int -> PPM
ppmBlur ppm i = undefined
--ppmBlur :: PPM -> Int -> PPM
--ppmBlur ppm = ppmBlurHelper ppm [] 0 

-- ppmBlurHelper :: PPM -> PPM -> Int -> PPM
-- ppmBlurHelper ppm blur i = ppmBlurRow ppm [] i 

-- ppmBlurRow :: PPM -> PPM -> Int -> Int -> PPM
-- ppmBlurRow ppm i j = 





getSubArray :: Int -> Int -> [a] -> [a]
getSubArray start end array = take (end - start + 1) (drop start array)

-- startRow, endRow, startCol, endCol
ppmCrop :: Int -> Int -> Int -> Int -> PPM -> PPM
ppmCrop r1 r2 c1 c2 ppm = 
  let rowCrop = getSubArray r1 r2 ppm in
  let colCrop = map (getSubArray c1 c2) rowCrop in
  colCrop
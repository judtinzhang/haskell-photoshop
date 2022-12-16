module Pixel
  ( RGBA (..),
    HSL (..),
    rgbToHsl,
    hslToRgb,
    pixelSaturate,
    mapRGBA,
    RGBARange (..),
    yellow,
    pixelChangeColor,
    pixelGrayScale,
    toPixel8,
    toDouble,
    ycbcrToRGBA,
    rgbToRGBA,
  )
where

import Codec.Picture
import GHC.Float as FL

type RGBA = (Double, Double, Double, Double)

type HSL = (Double, Double, Double, Double)

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
hslToRgb (h, s, l, a) = (r', g', b', a)
  where
    c = (1 - abs (2 * l - 1)) * s
    x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
    m = l - c / 2
    (r, g, b) = case h of
      h
        | h >= 0 && h < 60 -> (c, x, 0)
        | h >= 60 && h < 120 -> (x, c, 0)
        | h >= 120 && h < 180 -> (0, c, x)
        | h >= 180 && h < 240 -> (0, x, c)
        | h >= 240 && h < 300 -> (x, 0, c)
        | h >= 300 && h <= 360 -> (c, 0, x)
        | otherwise -> (0, 0, 0)
    r' = (r + m) * 255
    g' = (g + m) * 255
    b' = (b + m) * 255

-- Gets positive modulo
mod' :: Double -> Double -> Double
mod' x y =
  let m_x = double2Int x :: Int
   in let m_y = double2Int y :: Int
       in int2Double ((m_x `mod` m_y + m_y) `mod` m_y)

pixelSaturate :: Double -> RGBA -> RGBA
pixelSaturate deltaS p = p'
  where
    (h, s, l, a) = rgbToHsl p
    s' = max (min 1 deltaS) 0 -- keep saturation between 0 and 1
    p' = hslToRgb (h, s', l, a)

mapRGBA :: (Double -> a) -> RGBA -> (a, a, a, a)
mapRGBA f c = (f r, f g, f b, f a)
  where
    (r, g, b, a) = c

data RGBARange = RGBARange
  { redRange :: (Double, Double),
    greenRange :: (Double, Double),
    blueRange :: (Double, Double),
    alphaRange :: (Double, Double)
  }
  deriving (Show, Eq)

yellow :: RGBARange
yellow =
  RGBARange
    { redRange = (100, 255),
      greenRange = (100, 255),
      blueRange = (0, 150),
      alphaRange = (0, 255)
    }

pixelChangeColor :: RGBARange -> RGBA -> RGBA -> RGBA
pixelChangeColor cr t c =
  if rlow <= r
    && r <= rhigh
    && glow <= g
    && g <= ghigh
    && blow <= b
    && b <= bhigh
    && alow <= a
    && a <= ahigh
    then t
    else c
  where
    (rlow, rhigh) = redRange cr
    (glow, ghigh) = greenRange cr
    (blow, bhigh) = blueRange cr
    (alow, ahigh) = alphaRange cr
    (r, g, b, a) = c

pixelGrayScale :: RGBA -> RGBA
pixelGrayScale (r, g, b, a) = (gray, gray, gray, a)
  where
    gray = r / 3 + g / 3 + b / 3

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

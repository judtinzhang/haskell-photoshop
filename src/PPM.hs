module PPM
  ( Color (..),
    -- Pixel,
    PPM (..),
    ppmRotate,
    ppmReflectHorizontal,
    ppmReflectVertical,
    ppmChangeColor,
    ppmSaturate,
    ppmGrayscale,
    ppmBlur,
    ppmCrop,
  )
where

-- data Color = (,,) Int Int Int deriving (Eq)
newtype Color = C (Int, Int, Int) deriving (Eq, Show)

-- newtype Pixel = P Color
--   deriving (Eq)

type PPM =
  [[Color]]

ppmRotate :: PPM -> Int -> PPM
ppmRotate = undefined

ppmReflectHorizontal :: PPM -> PPM
ppmReflectHorizontal = undefined

ppmReflectVertical :: PPM -> PPM
ppmReflectVertical = undefined

ppmChangeColor :: PPM -> Color -> PPM
ppmChangeColor = undefined

ppmSaturate :: PPM -> Int -> PPM
ppmSaturate = undefined

ppmGrayscale :: PPM -> Int -> PPM
ppmGrayscale = undefined

ppmBlur :: PPM -> Int -> PPM
ppmBlur = undefined

-- ints: upper left corner (x, y) and size (w, l)
ppmCrop :: PPM -> Int -> Int -> Int -> Int -> PPM
ppmCrop = undefined
module Lib
  ( someFunc,
  )
where

import Graphics.Image qualified as Im

data IOFile
  = JPG String
  | PNG String

-- r g b
data Color = Color Int Int Int

type Pixel = Color

-- Location?

type PPM =
  [[Pixel]]

readInput :: IOFile -> PPM
readInput = undefined

-- Ints define scale at which we perform the action

rotate :: PPM -> Int -> PPM
rotate = undefined

changeColor :: PPM -> Color -> PPM
changeColor = undefined

saturate :: PPM -> Int -> PPM
saturate = undefined

grayscale :: PPM -> Int -> PPM
grayscale = undefined

blur :: PPM -> Int -> PPM
blur = undefined

-- ints: upper left corner (x, y) and size (w, l)
crop :: PPM -> Int -> Int -> Int -> Int -> PPM
crop = undefined

writeOutput :: PPM -> IO ()
writeOutput = undefined

someFunc :: String
someFunc = "Hello CIS 5520"

-- Where to go from here?
-- state transformations for modifications to PPM monad
-- concatenate images? Use monoid/semigroup
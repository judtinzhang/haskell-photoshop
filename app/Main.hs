module Main where

-- import PPM
--   ( blur,
--     changeColor,
--     crop,
--     grayscale,
--     readInput,
--     reflectHorizontal,
--     reflectVertical,
--     rotateLeft,
--     rotateRight,
--     saturate,
--     toJpg,
--     toPng,
--   )

import QuadTree
  ( blur,
    changeColor,
    crop,
    grayscale,
    lossyCompress,
    readInput,
    reflectHorizontal,
    reflectVertical,
    rotateLeft,
    rotateRight,
    saturate,
    toJpg,
    toPng,
  )

main :: IO ()
main = do
  -- image <- readInput "mountains.png"
  -- let transformed = rotateLeft image
  -- toPng transformed "output.png"

  -- image <- readInput "apple.png"
  -- let transformed = saturate 0.5 image
  -- toPng transformed "output.png"

  image <- readInput "car.png"
  let transformed = crop 0 585 0 640 $ grayscale $ reflectVertical image
  toPng transformed "output.png"

-- benchmarking
-- stack exec haskell-photoshop-exe

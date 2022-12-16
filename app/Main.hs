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
  image <- readInput "crab.png"

  let transformed = blur image 2
  -- let transformed = rotateLeft image

  toPng transformed "output.png"

-- -- let qt = changeColor yellow (100, 100, 255, 50) $ compress ppm
-- -- let qt = rotateRight $ grayscale $ reflectVertical $ compress ppm
-- -- let transformed = changeColor yellow (0, 0, 200, 255) ppm
-- -- let transformed = blur ppm 3
-- --  in let image = writeOutput transformed
-- --  in -- let transformed = crop 100 500 200 900 ppm
-- let qt = lossyCompress 60 ppm

-- benchmarking
-- stack exec project-cis5520-exe
-- clean up code

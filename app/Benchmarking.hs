import Criterion.Main
import PPM
  ( blur,
    changeColor,
    crop,
    grayscale,
    readInput,
    reflectHorizontal,
    reflectVertical,
    rotateLeft,
    rotateRight,
    saturate,
    toJpg,
    toPng,
  )

ppmRead :: IO ()
ppmRead = do
  image <- readInput "mountains.png"

ppmWrite :: IO ()
ppmWrite = do
  image <- readInput "mountains.png"
  toPng transformed "output.png"

ppmRotateRight :: IO ()
ppmRotateRight = do
  image <- readInput "mountains.png"
  let transformed = rotateLeft image

ppmReflectHorizontal :: IO ()
ppmReflectHorizontal = do
  image <- readInput "mountains.png"
  let transformed = ReflectHorizontal image

ppmReflectVertical :: IO ()
ppmReflectVertical = do
  image <- readInput "mountains.png"
  let transformed = ppmReflectVertical image

ppmSaturate :: IO ()
ppmSaturate = do
  image <- readInput "mountains.png"
  let transformed = ppmSaturate image

ppmBlur :: IO ()
ppmBlur = do
  image <- readInput "crab.png"
  let transformed = ppmBlur 2 image

main =
  defaultMain
    [ bgroup
        "ppm"
        [ bench "rotLeft" $ nfIO ppmRotateLeft,
          bench "rotRight" $ nfIO ppmRotateLeft, 
          bench "reflectHorizontal" $ nfIO ppmReflectHorizontal, 
          bench "reflectVertical" $ nfIO ppmReflectVertical, 
          bench "ppmSaturate" $ nfIO ppmSaturate, 
          bench "rotRight" $ nfIO ppmRotateLeft, 
        ]
    ]
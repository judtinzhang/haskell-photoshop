import Criterion.Main
import qualified PPM as P
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
import Pixel
import qualified QuadTree as QT
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

ppmWrite :: IO ()
ppmWrite = do
  image <- P.readInput "mountains.png"
  P.toPng image "output.png"

ppmChangeColor :: IO ()
ppmChangeColor = do
  image <- P.readInput "mountains.png"
  let transformed = P.changeColor yellow (0, 255, 0, 255) image
  P.toPng transformed "output.png"

ppmRotateLeft :: IO ()
ppmRotateLeft = do
  image <- P.readInput "mountains.png"
  let transformed = P.rotateLeft image
  P.toPng image "output.png"

ppmRotateRight :: IO ()
ppmRotateRight = do
  image <- P.readInput "mountains.png"
  let transformed = P.rotateRight image
  P.toPng image "output.png"

ppmReflectHorizontal :: IO ()
ppmReflectHorizontal = do
  image <- P.readInput "mountains.png"
  let transformed = P.reflectHorizontal image
  P.toPng image "output.png"

ppmReflectVertical :: IO ()
ppmReflectVertical = do
  image <- P.readInput "mountains.png"
  let transformed = P.reflectVertical image
  P.toPng image "output.png"

ppmSaturate :: IO ()
ppmSaturate = do
  image <- P.readInput "mountains.png"
  let transformed = P.saturate 1 image
  P.toPng image "output.png"

ppmBlur :: IO ()
ppmBlur = do
  image <- P.readInput "mountains.png"
  let transformed = P.blur image 2
  P.toPng image "output.png"

  qtWrite :: IO ()

qtWrite = do
  image <- QT.readInput "mountains.png"
  QT.toPng image "output.png"

qtChangeColor :: IO ()
qtChangeColor = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.changeColor yellow (0, 255, 0, 255) image
  QT.toPng transformed "output.png"

qtRotateLeft :: IO ()
qtRotateLeft = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.rotateLeft image
  QT.toPng image "output.png"

qtRotateRight :: IO ()
qtRotateRight = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.rotateRight image
  QT.toPng image "output.png"

qtReflectHorizontal :: IO ()
qtReflectHorizontal = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.reflectHorizontal image
  QT.toPng image "output.png"

qtReflectVertical :: IO ()
qtReflectVertical = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.reflectVertical image
  QT.toPng image "output.png"

qtSaturate :: IO ()
qtSaturate = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.saturate 1 image
  QT.toPng image "output.png"

qtBlur :: IO ()
qtBlur = do
  image <- QT.readInput "mountains.png"
  let transformed = QT.blur image 2
  QT.toPng image "output.png"

main =
  defaultMain
    [ bgroup
        "ppm"
        [ bench "write" $ nfIO ppmWrite,
          bench "rotLeft" $ nfIO ppmRotateLeft,
          bench "rotRight" $ nfIO ppmRotateRight,
          bench "reflectHorizontal" $ nfIO ppmReflectHorizontal,
          bench "reflectVertical" $ nfIO ppmReflectVertical,
          bench "saturate" $ nfIO ppmSaturate,
          bench "blur" $ nfIO ppmBlur
        ],
      bgroup
        "qt"
        [ bench "write" $ nfIO qtWrite,
          bench "rotLeft" $ nfIO qtRotateLeft,
          bench "rotRight" $ nfIO qtRotateRight,
          bench "reflectHorizontal" $ nfIO qtReflectHorizontal,
          bench "reflectVertical" $ nfIO qtReflectVertical,
          bench "saturate" $ nfIO qtSaturate,
          bench "blur" $ nfIO qtBlur
        ]
    ]
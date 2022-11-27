import IO
import PPM
import QuadTree
import Test.HUnit
import Test.QuickCheck

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = undefined
  shrink :: Color -> [Color]
  shrink _ = []

instance (Arbitrary e) => Arbitrary (QuadTree e) where
  arbitrary :: Gen (QuadTree e)
  arbitrary = undefined
  shrink :: QuadTree e -> [QuadTree e]
  shrink (QT a b c d _) = [a, b, c, d]
  shrink _ = []

-- Decompress and then compress should yield same QuadTree
propDecompressCompressValid :: QuadTree Color -> Bool
propDecompressCompressValid qt = qt == compress (decompress qt)

-- Compress and then decompress should yield same PPM
propCompressDecompressValid :: PPM -> Bool
propCompressDecompressValid ppm = ppm == decompress (compress ppm)

-- Property tests assert that operators should output the same result
-- regardless of image representation (PPM and QuadTree)

propRotate90 :: QuadTree Color -> Bool
propRotate90 qt = qtRotate qt 90 == compress (ppmRotate (decompress qt) 90)

propRotate180 :: QuadTree Color -> Bool
propRotate180 qt = qtRotate qt 180 == compress (ppmRotate (decompress qt) 180)

propRotate270 :: QuadTree Color -> Bool
propRotate270 qt = qtRotate qt 270 == compress (ppmRotate (decompress qt) 270)

propReflectHorizontal :: QuadTree Color -> Bool
propReflectHorizontal qt = qtReflectHorizontal qt == compress (ppmReflectHorizontal (decompress qt))

propReflectVertical :: QuadTree Color -> Bool
propReflectVertical qt = qtReflectVertical qt == compress (ppmReflectVertical (decompress qt))

propChangeColor :: QuadTree Color -> Color -> Bool
propChangeColor qt color = qtChangeColor qt color == compress (ppmChangeColor (decompress qt) color)

propSaturate :: QuadTree Color -> Int -> Bool
propSaturate qt x = qtSaturate qt x == compress (ppmSaturate (decompress qt) x)

propGrayScale :: QuadTree Color -> Int -> Bool
propGrayScale qt x = qtGrayscale qt x == compress (ppmGrayscale (decompress qt) x)

propBlur :: QuadTree Color -> Int -> Bool
propBlur qt x = qtBlur qt x == compress (ppmBlur (decompress qt) x)

propCrop :: QuadTree Color -> Int -> Int -> Int -> Int -> Bool
propCrop qt x y z w = qtCrop qt x y z w == compress (ppmCrop (decompress qt) x y z w)

-- Begint test cases

white :: Color
white = C (256, 256, 256)

whitePPM :: PPM
whitePPM = [[white, white], [white, white]]

whiteQT :: QuadTree Color
whiteQT = Leaf white

black :: Color
black = C (0, 0, 0)

whiteBlackPPM :: PPM
whiteBlackPPM = [[black, white], [white, white]]

whiteBlackQT :: QuadTree Color
whiteBlackQT = QT (Leaf black) (Leaf white) (Leaf white) (Leaf white) 1

testCompress :: Test
testCompress =
  "Compress"
    ~: TestList
      [ compress whitePPM ~?= whiteQT,
        compress whiteBlackPPM ~?= whiteBlackQT
      ]

testDecompress :: Test
testDecompress =
  "Decompress"
    ~: TestList
      [ decompress whiteQT ~?= whitePPM,
        decompress whiteBlackQT ~?= whiteBlackPPM
      ]

testRotate :: Test
testRotate =
  "Rotate"
    ~: TestList
      [ qtRotate whiteQT 90 ~?= whiteQT,
        ppmRotate whitePPM 180 ~?= whitePPM,
        qtRotate whiteBlackQT 90
          ~?= QT (Leaf white) (Leaf black) (Leaf white) (Leaf white) 1,
        ppmRotate whiteBlackPPM 90 ~?= [[white, black], [white, white]]
      ]

testReflect :: Test
testReflect =
  "Reflect"
    ~: TestList
      [ qtReflectHorizontal whiteQT ~?= whiteQT,
        ppmReflectVertical whitePPM ~?= whitePPM,
        qtReflectVertical whiteBlackQT
          ~?= QT (Leaf white) (Leaf white) (Leaf black) (Leaf white) 1,
        ppmReflectHorizontal whiteBlackPPM ~?= [[white, black], [white, white]]
      ]

test_all :: IO Counts
test_all = runTestTT $ TestList [testCompress, testDecompress, testRotate, testReflect]

qc :: IO ()
qc = do
  putStrLn "Decompress Compress"
  quickCheck propDecompressCompressValid
  putStrLn "Compress Decompress"
  quickCheck propCompressDecompressValid
  putStrLn "Rotate 90"
  quickCheck propRotate90
  putStrLn "Rotate 180"
  quickCheck propRotate180
  putStrLn "Rotate 270"
  quickCheck propRotate270
  putStrLn "Reflect Horizontal"
  quickCheck propReflectHorizontal
  putStrLn "Reflect Vertical"
  quickCheck propReflectVertical
  putStrLn "Change Color"
  quickCheck propChangeColor
  putStrLn "Saturate"
  quickCheck propSaturate
  putStrLn "Grayscale"
  quickCheck propGrayScale
  putStrLn "Blur"
  quickCheck propBlur
  putStrLn "Crop"
  quickCheck propCrop

main :: IO ()
main =
  do
    test_all
    qc
    putStrLn "Test suite not yet implemented"
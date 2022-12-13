import PPM
import Pixel
import QuadTree
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck (Arbitrary (arbitrary))

newtype RGBAWrapper = RGBAW {rgba :: RGBA}

instance Arbitrary RGBAWrapper where
  arbitrary = do
    r <- choose (1, 5)
    g <- choose (1, 5)
    b <- choose (1, 5)
    a <- choose (1, 5)
    return $ RGBAW {rgba = (r, g, b, a)}

newtype PPMWrapper = PPMW {ppm :: PPM}
  deriving (Show)

instance Arbitrary PPMWrapper where
  arbitrary :: Gen PPMWrapper
  arbitrary = do
    qt <- arbitrary :: Gen (QuadTree RGBA)
    return $ PPMW $ decompress qt

  shrink :: PPMWrapper -> [PPMWrapper]
  shrink _ = []

genRGBARange :: Gen (Double, Double)
genRGBARange = do
  x <- choose (0, 255)
  y <- choose (0, 255)
  if x < y then return (x, y) else return (y, x)

instance Arbitrary RGBARange where
  arbitrary = do
    rRange <- genRGBARange
    gRange <- genRGBARange
    bRange <- genRGBARange
    aRange <- genRGBARange
    return $
      RGBARange
        { redRange = rRange,
          greenRange = gRange,
          blueRange = bRange,
          alphaRange = aRange
        }

  -- arbitrary = RGBARange <$> (arbitrary, arbitrary, arbitrary, arbitrary)
  shrink :: RGBARange -> [RGBARange]
  shrink _ = []

genQuadTree :: Int -> Int -> Gen (QuadTree RGBA)
genQuadTree 1 1 = genLeaf 1 1
genQuadTree 1 w = do
  pData <- vectorOf w (do rgba <$> arbitrary)
  return $ LeafList PL {isHorizontal = True, pixelData = pData}
genQuadTree h 1 = do
  pData <- vectorOf h (do rgba <$> arbitrary)
  return $ LeafList PL {isHorizontal = False, pixelData = pData}
genQuadTree h w =
  frequency
    [ (h + w, genQT h w),
      (1, genLeaf h w)
    ]

genQT :: Int -> Int -> Gen (QuadTree RGBA)
genQT h w = do
  let w_len_1 = w `div` 2
  let w_len_2 = w - w_len_1
  let h_len_1 = h `div` 2
  let h_len_2 = h - h_len_1
  tl <- genQuadTree h_len_1 w_len_1
  tr <- genQuadTree h_len_1 w_len_2
  bl <- genQuadTree h_len_2 w_len_1
  br <- genQuadTree h_len_2 w_len_2
  return $ QT tl tr bl br h w

genLeaf :: Int -> Int -> Gen (QuadTree RGBA)
genLeaf h w = do
  wrapper <- arbitrary :: Gen RGBAWrapper
  return $ Leaf (rgba wrapper, h, w)

instance Arbitrary (QuadTree RGBA) where
  arbitrary :: Gen (QuadTree RGBA)
  arbitrary = do
    w <- suchThat arbitrary (> 0)
    h <- suchThat arbitrary (> 0)
    genQuadTree h w

  shrink :: QuadTree RGBA -> [QuadTree RGBA]
  shrink (QT tl tr bl br _ _) = [tl, tr, bl, br]
  shrink _ = []

-- Begin QuickCheck

-- Decompress and then compress should yield same QuadTree
propDecompressCompressValid :: QuadTree RGBA -> Bool
propDecompressCompressValid qt = qt == compress (decompress qt)

propRotateLeft :: QuadTree RGBA -> Bool
propRotateLeft qt = decompress (qtRotateLeft qt) == ppmRotateLeft (decompress qt)

-- Compress and then decompress should yield same PPM
propCompressDecompressValid :: PPMWrapper -> Bool
propCompressDecompressValid ppmw = ppm ppmw == decompress (compress $ ppm ppmw)

-- Property tests assert that operators should output the same result
-- regardless of image representation (PPM and QuadTree)

propRotateRight :: QuadTree RGBA -> Bool
propRotateRight qt = decompress (qtRotateRight qt) == ppmRotateRight (decompress qt)

propReflectHorizontal :: QuadTree RGBA -> Bool
propReflectHorizontal qt = decompress (qtReflectHorizontal qt) == ppmReflectHorizontal (decompress qt)

propReflectVertical :: QuadTree RGBA -> Bool
propReflectVertical qt = decompress (qtReflectVertical qt) == ppmReflectVertical (decompress qt)

propChangeColor :: RGBARange -> RGBA -> QuadTree RGBA -> Bool
propChangeColor range target qt = qtChangeColor range target qt == compress (ppmChangeColor range target (decompress qt))

propSaturate :: Double -> QuadTree RGBA -> Bool
propSaturate x qt = qtSaturate x qt == compress (ppmSaturate x (decompress qt))

propGrayScale :: QuadTree RGBA -> Bool
propGrayScale qt = qtGrayscale qt == compress (ppmGrayscale (decompress qt))

propBlur :: QuadTree RGBA -> Int -> Bool
propBlur qt x = qtBlur qt x == compress (ppmBlur (decompress qt) x)

propCrop :: QuadTree RGBA -> Int -> Int -> Int -> Int -> Bool
propCrop qt x y z w = qtCrop x y z w qt == compress (ppmCrop x y z w (decompress qt))

-- Begin test cases

white :: RGBA
white = (255, 255, 255, 255)

whitePPM :: PPM
whitePPM = [[white, white], [white, white]]

whiteQT :: QuadTree RGBA
whiteQT = Leaf (white, 2, 2)

black :: RGBA
black = (0, 0, 0, 255)

whiteBlackPPM :: PPM
whiteBlackPPM = [[black, white], [white, white]]

whiteBlackQT :: QuadTree RGBA
whiteBlackQT = QT (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) 2 2

advancedQT :: QuadTree RGBA
advancedQT =
  QT
    (Leaf ((137, 223, 68, 138), 1, 1))
    (LeafList (PL {isHorizontal = True, pixelData = [(3, 216, 177, 47), (60, 210, 173, 87)]}))
    (Leaf ((236, 59, 104, 236), 1, 1))
    (LeafList (PL {isHorizontal = True, pixelData = [(81, 181, 38, 120), (173, 39, 57, 73)]}))
    2
    3

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
      [ qtRotateLeft whiteQT ~?= whiteQT,
        ppmRotateRight whitePPM ~?= whitePPM,
        qtRotateRight whiteBlackQT
          ~?= QT (Leaf (white, 1, 1)) (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) 2 2,
        ppmRotateRight
          whiteBlackPPM
          ~?= [[white, black], [white, white]]
      ]

testReflect :: Test
testReflect =
  "Reflect"
    ~: TestList
      [ qtReflectHorizontal whiteQT ~?= whiteQT,
        ppmReflectVertical whitePPM ~?= whitePPM,
        qtReflectVertical whiteBlackQT
          ~?= QT (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) 2 2,
        ppmReflectHorizontal whiteBlackPPM ~?= [[white, black], [white, white]]
      ]

testGrayScale :: Test
testGrayScale =
  "Gray Scale"
    ~: TestList
      [ qtGrayscale advancedQT ~?= compress (ppmGrayscale (decompress advancedQT)),
        qtGrayscale whiteQT ~?= compress (ppmGrayscale (decompress whiteQT))
      ]

test_all :: IO Counts
test_all = runTestTT $ TestList [testCompress, testDecompress, testRotate, testReflect, testGrayScale]

qc :: IO ()
qc = do
  putStrLn "Decompress Compress"
  quickCheck propDecompressCompressValid
  putStrLn "Compress Decompress"
  quickCheck propCompressDecompressValid
  putStrLn "Rotate Left"
  quickCheck propRotateLeft
  putStrLn "Rotate Right"
  quickCheck propRotateRight
  putStrLn "Reflect Horizontal"
  quickCheck propReflectHorizontal
  putStrLn "Reflect Vertical"
  quickCheck propReflectVertical
  putStrLn "Change RGBA"
  quickCheck propChangeColor
  putStrLn "Saturate"
  quickCheck propSaturate
  putStrLn "Grayscale"
  quickCheck propGrayScale

-- putStrLn "Blur"
-- quickCheck propBlur
-- putStrLn "Crop"
-- quickCheck propCrop

main :: IO ()
main = do
  test_all
  qc
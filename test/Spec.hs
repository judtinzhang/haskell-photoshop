import PPM qualified as P
import Pixel
import QuadTree (QuadTree(QT, Leaf, LeafList), PixelList (..), recompress, compress, decompress)
import QuadTree qualified as QT
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck

newtype RGBAWrapper = RGBAW {rgba :: RGBA}

rounded :: Double -> Double
rounded x = fromIntegral $ round x

instance Arbitrary RGBAWrapper where
  arbitrary :: Gen RGBAWrapper
  arbitrary = do
    r <- choose (4, 5)
    g <- choose (4, 5)
    b <- choose (4, 5)
    a <- choose (4, 5)
    return $ RGBAW {rgba = (rounded r, rounded g, rounded b, rounded a)}

newtype PPMWrapper = PPMW {ppm :: P.PPM}
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
    [ (10 * (h + w), genQT h w),
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

  shrink :: QT.QuadTree RGBA -> [QuadTree RGBA]
  shrink (QT tl tr bl br _ _) = [tl, tr, bl, br]
  shrink _ = []

-- Begin QuickCheck

-- Decompress and then compress should yield same QuadTree
propDecompressCompressValid :: QT.QuadTree RGBA -> Bool
propDecompressCompressValid qt = recompress qt == compress (decompress qt)

propRotateLeft :: QT.QuadTree RGBA -> Bool
propRotateLeft qt = decompress (QT.rotateLeft qt) == P.rotateLeft (decompress qt)

-- The QuadTree format should maintain the same size before and after compression
propLossless :: PPMWrapper -> Bool
propLossless ppmw =
  let oldPPM = ppm ppmw
   in let newPPM = decompress (compress (ppm ppmw))
       in length oldPPM * length (head oldPPM)
            == length newPPM * length (head newPPM)

-- Compress and then decompress should yield same PPM
propCompressDecompressValid :: PPMWrapper -> Bool
propCompressDecompressValid ppmw = ppm ppmw == decompress (compress $ ppm ppmw)

-- Property tests assert that operators should output the same result
-- regardless of image representation (PPM and QuadTree)

propRotateRight :: QT.QuadTree RGBA -> Bool
propRotateRight qt = decompress (QT.rotateRight qt) == P.rotateRight (decompress qt)

propReflectHorizontal :: QT.QuadTree RGBA -> Bool
propReflectHorizontal qt = decompress (QT.reflectHorizontal qt) == P.reflectHorizontal (decompress qt)

propReflectVertical :: QT.QuadTree RGBA -> Bool
propReflectVertical qt = decompress (QT.reflectVertical qt) == P.reflectVertical (decompress qt)

propChangeColor :: RGBARange -> RGBA -> QT.QuadTree RGBA -> Bool
propChangeColor range target qt = decompress (QT.changeColor range target qt) == P.changeColor range target (decompress qt)

propSaturate :: Double -> QT.QuadTree RGBA -> Bool
propSaturate x qt = decompress (QT.saturate x qt) == P.saturate x (decompress qt)

propGrayScale :: QT.QuadTree RGBA -> Bool
propGrayScale qt = decompress (QT.grayscale qt) == P.grayscale (decompress qt)

propBlur :: QT.QuadTree RGBA -> Int -> Property
propBlur qt x = x > 0 ==> decompress (QT.blur qt (min 5 x)) == P.blur (decompress qt) (min 5 x)

validRanges :: P.PPM -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
validRanges ppm a b c d =
  let r1 = min (length ppm - 1) $ max 0 a
   in let r2 = min (length ppm - 1) $ max 0 b
       in let c1 = min (length (head ppm) - 1) $ max 0 c
           in let c2 = min (length (head ppm) - 1) $ max 0 d
               in (min r1 r2, max r1 r2, min c1 c2, max c1 c2)

-- Test propCrop on PPM because QuadTree does not maintain absolute coordinates of pixels
propCrop :: PPMWrapper -> Int -> Int -> Int -> Int -> Bool
propCrop ppmw a b c d =
  let p = ppm ppmw
   in let (r1, r2, c1, c2) = validRanges p a b c d
       in decompress (QT.crop r1 r2 c1 c2 (compress p))
            == P.crop r1 r2 c1 c2 p

-- Test that getting an x y coordinate in both QuadTree and PPM yield same value
propGetColor :: PPMWrapper -> Int -> Int -> Int -> Int -> Bool
propGetColor ppmw a b c d =
  let p = ppm ppmw
   in let (r1, r2, c1, c2) = validRanges p a b c d
       in Just (p !! r1 !! c1) == QT.getColor r1 c1 (compress p)
            && Just (p !! r2 !! c2) == QT.getColor r2 c2 (compress p)

-- Begin test cases

white :: RGBA
white = (255, 255, 255, 255)

whitePPM :: P.PPM
whitePPM = [[white, white], [white, white]]

whiteQT :: QT.QuadTree RGBA
whiteQT = Leaf (white, 2, 2)

black :: RGBA
black = (0, 0, 0, 255)

whiteBlackPPM :: P.PPM
whiteBlackPPM = [[black, white], [white, white]]

whiteBlackQT :: QT.QuadTree RGBA
whiteBlackQT = QT (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) 2 2

advancedQT :: QT.QuadTree RGBA
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
      [ QT.rotateLeft whiteQT ~?= whiteQT,
        P.rotateRight whitePPM ~?= whitePPM,
        QT.rotateRight whiteBlackQT
          ~?= QT (Leaf (white, 1, 1)) (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) 2 2,
        P.rotateRight
          whiteBlackPPM
          ~?= [[white, black], [white, white]]
      ]

testReflect :: Test
testReflect =
  "Reflect"
    ~: TestList
      [ QT.reflectHorizontal whiteQT ~?= whiteQT,
        P.reflectVertical whitePPM ~?= whitePPM,
        QT.reflectVertical whiteBlackQT
          ~?= QT (Leaf (white, 1, 1)) (Leaf (white, 1, 1)) (Leaf (black, 1, 1)) (Leaf (white, 1, 1)) 2 2,
        P.reflectHorizontal whiteBlackPPM ~?= [[white, black], [white, white]]
      ]

testGrayScale :: Test
testGrayScale =
  "Gray Scale"
    ~: TestList
      [ QT.grayscale advancedQT ~?= compress (P.grayscale (decompress advancedQT)),
        QT.grayscale whiteQT ~?= compress (P.grayscale (decompress whiteQT))
      ]

-- tPT = QT (Leaf ((2.0, 4.0, 4.0, 4.0), 1, 1)) (Leaf ((2.0, 1.0, 5.0, 3.0), 1, 1)) (Leaf ((3.0, 3.0, 1.0, 3.0), 1, 1)) (Leaf ((4.0, 3.0, 2.0, 1.0), 1, 1)) 2 2

-- QT.getColor (-1) (-1) tPT
-- testBad :: Test
-- testBad =
--   "testBad"
--     ~: TestList
--       [decompress (QT.blur tPT 1) ~?= blur (decompress tPT) 1]

test_all :: IO Counts
test_all = runTestTT $ TestList [testCompress, testDecompress, testRotate, testReflect, testGrayScale]

-- test_all = runTestTT $ TestList [testBad]

-- [[(2.5,3.0,3.0,2.5),(2.5,3.0,3.0,2.5)]]
qc :: IO ()
qc = do
  putStrLn "Decompress Compress"
  quickCheck propDecompressCompressValid
  putStrLn "Compress Decompress"
  quickCheck propCompressDecompressValid
  putStrLn "Lossless"
  quickCheck propLossless
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
  putStrLn "Blur"
  quickCheck propBlur
  putStrLn "Crop"
  quickCheck propCrop
  putStrLn "Get Color"
  quickCheck propGetColor

main :: IO ()
main = do
  test_all
  qc
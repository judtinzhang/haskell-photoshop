{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

import Data.Vector (create)
import Data.Vector qualified as V
import IO
import PPM
import Pixel
import QuadTree
import System.Random
import Test.HUnit
import Test.QuickCheck (Arbitrary (arbitrary, shrink), quickCheck)
import Test.QuickCheck qualified as QC

newtype RGBAWrapper = RGBAW
  { rgba :: RGBA
  }

instance Arbitrary RGBAWrapper where
  arbitrary = do
    r <- QC.choose (0, 255)
    g <- QC.choose (0, 255)
    b <- QC.choose (0, 255)
    a <- QC.choose (0, 255)
    return $ RGBAW {rgba = (r, g, b, a)}

newtype PPMWrapper = PPMW
  { ppm :: PPM
  }

instance Arbitrary PPMWrapper where
  arbitrary = do
    -- pData <- QC.vectorOf w (do rgba <$> arbitrary)
    return $ PPMW []

instance Arbitrary RGBARange where
  arbitrary = undefined

  -- arbitrary = RGBARange <$> (arbitrary, arbitrary, arbitrary, arbitrary)
  shrink :: RGBARange -> [RGBARange]
  shrink _ = []

genQuadTree :: Int -> Int -> QC.Gen (QuadTree RGBA)
genQuadTree 1 1 = genLeaf 1 1
genQuadTree 1 w = do
  pData <- QC.vectorOf w (do rgba <$> arbitrary)
  return $ LeafList PL {isHorizontal = True, pixelData = pData}
genQuadTree h 1 = do
  pData <- QC.vectorOf h (do rgba <$> arbitrary)
  return $ LeafList PL {isHorizontal = False, pixelData = pData}
genQuadTree h w =
  QC.frequency
    [ (h + w, genQT h w),
      (1, genLeaf h w)
    ]

-- Bug: QuadTree doesn't generate leaves that are not squares
-- Bug: QuadTree doesn't combine pixelLists and leaves of same color

genQT :: Int -> Int -> QC.Gen (QuadTree RGBA)
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

genLeaf :: Int -> Int -> QC.Gen (QuadTree RGBA)
genLeaf h w = do
  wrapper <- arbitrary :: QC.Gen RGBAWrapper
  return $ Leaf (rgba wrapper, h, w)

instance Arbitrary (QuadTree RGBA) where
  arbitrary :: QC.Gen (QuadTree RGBA)
  arbitrary = do
    w <- QC.suchThat arbitrary (> 0)
    h <- QC.suchThat arbitrary (> 0)
    genQuadTree h w

  shrink :: QuadTree RGBA -> [QuadTree RGBA]
  shrink (QT tl tr bl br _ _) = [tl, tr, bl, br]
  shrink _ = []

-- Decompress and then compress should yield same QuadTree
propDecompressCompressValid :: QuadTree RGBA -> Bool
propDecompressCompressValid qt = qt == compress (decompress qt)

propRotateLeft :: QuadTree RGBA -> Bool
propRotateLeft qt = decompress (qtRotateLeft qt) == ppmRotateLeft (decompress qt)

-- -- Compress and then decompress should yield same PPM
-- propCompressDecompressValid :: PPM -> Bool
-- propCompressDecompressValid ppm = ppm == decompress (compress ppm)

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

bad :: QuadTree RGBA
bad = QT (Leaf ((137.61239733587485, 223.36502706515898, 68.28475064036621, 138.50580976970878), 1, 1)) (LeafList (PL {isHorizontal = True, pixelData = [(3.089512996552081, 216.7821256728732, 177.99180609349364, 47.44186640041945), (60.07791449643962, 210.77062666812282, 173.82683671966993, 87.66751296892083)]})) (Leaf ((236.4446672977997, 59.76664264570588, 104.92565927222985, 236.80402599226608), 1, 1)) (LeafList (PL {isHorizontal = True, pixelData = [(81.32314569596649, 181.41484790059405, 38.537272388234, 120.23963543855334), (173.83028039169733, 39.61689084185856, 57.82888766556308, 73.98461387010298)]})) 2 3

testBad :: Test
testBad =
  "bad"
    ~: TestList
      [ qtReflectHorizontal bad ~?= compress (ppmReflectHorizontal (decompress bad)),
        qtReflectHorizontal bad ~?= whiteQT
      ]

test_woke :: IO Counts
test_woke = runTestTT $ TestList [testBad]

test_all :: IO Counts
test_all = runTestTT $ TestList [testCompress, testDecompress, testRotate, testReflect]

qc :: IO ()
qc = do
  putStrLn "Decompress Compress"
  quickCheck propDecompressCompressValid
  -- putStrLn "Compress Decompress"
  -- quickCheck propCompressDecompressValid
  putStrLn "Rotate Left"
  quickCheck propRotateLeft
  putStrLn "Rotate Right"
  quickCheck propRotateRight
  putStrLn "Reflect Horizontal"
  quickCheck propReflectHorizontal
  putStrLn "Reflect Vertical"
  quickCheck propReflectVertical
  -- putStrLn "Change RGBA"
  -- quickCheck propChangeColor
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
  qc

-- test_woke
-- test_all
-- print "hi"
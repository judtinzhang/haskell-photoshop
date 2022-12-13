module QuadTree (
    QuadTree (..),
    PixelList (..),
    compress,
    decompress,
    qtRotateLeft,
    qtRotateRight,
    qtReflectHorizontal,
    qtReflectVertical,
    qtChangeColor,
    qtSaturate,
    qtGrayscale,
    qtBlur,
    qtCrop,
    testQT,
) where

import Data.Foldable qualified as Foldable
import PPM qualified as P (PPM)
import Pixel
import Debug.Trace

-- ================= DEBUGGING =========================

mat :: [[Int]]
mat = [[1, 1, 1, 4], [1, 3, 3, 4], [1, 3, 3, 5]]

qt :: QuadTree Int
qt = buildQuadTree mat

testQT :: IO ()
testQT = do
  -- print qt
  let ppm = buildPPM $ fmap (+2) qt
  print ppm

-- =====================================================

data PixelList e = PL {
  isHorizontal :: Bool,
  pixelData :: [e]
} deriving (Show, Eq)

instance Foldable PixelList where
  foldMap f pl = foldMap f (pixelData pl)

instance Functor PixelList where
  fmap f pl = PL {
    isHorizontal = isHorizontal pl,
    pixelData = map f (pixelData pl)
  }

data QuadTree e
  = Leaf (e, Int, Int)
  | LeafList (PixelList e)
  | QT -- non-empty tree, with...
      (QuadTree e)
      (QuadTree e)
      (QuadTree e)
      (QuadTree e)
      Int -- height
      Int -- width
  deriving (Show, Foldable, Eq, Functor)

getSubArray :: Int -> Int -> [a] -> [a]
getSubArray start end array = take (end - start) (drop start array)

-- x1, y2 are inclusive, x2, y2 exclusive
getSubMatrix :: Int -> Int -> Int -> Int -> [[a]] -> [[a]]
getSubMatrix x1 x2 y1 y2 ppm = map (getSubArray x1 x2) (getSubArray y1 y2 ppm)

sameQTColor :: Eq e => QuadTree e -> QuadTree e -> Bool
sameQTColor (Leaf (x, _, _)) (Leaf (y, _, _)) = x == y
-- sameQTColor (Leaf (x, _, _)) (LeafList pl) =
--   let (b, c) = colorPL pl in
--   b && (c == x)
-- sameQTColor (LeafList pl) (Leaf (y, _, _)) =
--   let (b, c) = colorPL pl in
--   b && (c == y)
-- sameQTColor (LeafList pl1) (LeafList pl2) =
--   let (b1, c1) = colorPL pl1 in
--   let (b2, c2) = colorPL pl2 in
--   b1 && b2 && (c1 == c2)
sameQTColor _ _ = False

color :: QuadTree e -> Maybe e
color (Leaf (x, _, _)) = Just x
-- color (LeafList pl) =
--   let (b, c) = colorPL pl in
--     if b then Just c else Nothing
color _ = Nothing

-- colorPL :: (Eq e, Show e) => PixelList e -> (Bool, e)
-- colorPL pl = traceShow pl foldr (\x (b, c) -> ((x == c) && b, c)) (True, (pixelData pl) !! 0) (pixelData pl)

buildQuadTree :: Eq e => [[e]] -> QuadTree e
buildQuadTree ppm@(w : ws) =
  let y_len = length ppm in
  let x_len = length w in
  let x_mid = x_len `div` 2  in
  let y_mid = y_len `div` 2 in
  if x_len == 1 && y_len == 1
    then Leaf (head $ head ppm, 1, 1)
    else
      if x_len == 1 || y_len == 1
        then
          LeafList PL {
            isHorizontal = y_len == 1,
            pixelData = concat ppm
          }
        else
          let tl = buildQuadTree $ getSubMatrix 0 x_mid 0 y_mid ppm in
          let tr = buildQuadTree $ getSubMatrix x_mid x_len 0 y_mid ppm in
          let bl = buildQuadTree $ getSubMatrix 0 x_mid y_mid y_len ppm in
          let br = buildQuadTree $ getSubMatrix x_mid x_len y_mid y_len ppm in
          if sameQTColor tl tr && sameQTColor tr bl && sameQTColor bl br
            then case color tl of
              Just c -> Leaf (c, y_len, x_len)
              Nothing -> error "impossible color"
            else QT tl tr bl br (length ppm) (length w)
buildQuadTree [] = error "QT cannot accept empty image"

combinePPM :: [[e]] -> [[e]] -> [[e]] -> [[e]] -> [[e]]
combinePPM tl tr bl br = zipWith (++) (tl ++ bl) (tr ++ br)

buildPPM :: Eq e => QuadTree e -> [[e]]
buildPPM (Leaf (x, i, j)) = replicate i $ replicate j x
buildPPM (LeafList pl) =
  if isHorizontal pl
    then [pixelData pl]
    else map (: []) $ pixelData pl
buildPPM (QT tl tr bl br i j) =
  let ppm1 = buildPPM tl in
  let ppm2 = buildPPM tr in
  let ppm3 = buildPPM bl in
  let ppm4 = buildPPM br in
  combinePPM ppm1 ppm2 ppm3 ppm4

compress :: P.PPM -> QuadTree RGBA
compress = buildQuadTree

decompress :: QuadTree RGBA -> P.PPM
decompress = buildPPM

qtRotateLeft :: QuadTree e -> QuadTree e
qtRotateLeft (Leaf x) = Leaf x
qtRotateLeft (LeafList pl) =
    LeafList PL {
      isHorizontal = not $ isHorizontal pl,
      pixelData = if isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
qtRotateLeft (QT tl tr bl br h w) = QT
  (qtRotateLeft tr)
  (qtRotateLeft br)
  (qtRotateLeft tl)
  (qtRotateLeft bl)
  w
  h

qtRotateRight :: QuadTree e -> QuadTree e
qtRotateRight (Leaf x) = Leaf x
qtRotateRight (LeafList pl) =
    LeafList PL {
      isHorizontal = not $ isHorizontal pl,
      pixelData = if not $ isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
qtRotateRight (QT tl tr bl br h w) = QT
  (qtRotateRight bl)
  (qtRotateRight tl)
  (qtRotateRight br)
  (qtRotateRight tr)
  w
  h

qtReflectHorizontal :: QuadTree e -> QuadTree e
qtReflectHorizontal (Leaf x) = Leaf x
qtReflectHorizontal (LeafList pl) =
    LeafList PL {
      isHorizontal = isHorizontal pl,
      pixelData = if isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
qtReflectHorizontal (QT tl tr bl br h w) = QT
  (qtReflectHorizontal tr)
  (qtReflectHorizontal tl)
  (qtReflectHorizontal br)
  (qtReflectHorizontal bl)
  h
  w

qtReflectVertical :: QuadTree e -> QuadTree e
qtReflectVertical (Leaf x) = Leaf x
qtReflectVertical (LeafList pl) =
    LeafList PL {
      isHorizontal = isHorizontal pl,
      pixelData = if not $ isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
qtReflectVertical (QT tl tr bl br h w) = QT
  (qtReflectVertical bl)
  (qtReflectVertical br)
  (qtReflectVertical tl)
  (qtReflectVertical tr)
  h
  w

-- TODO: have a recompress function for things like this
qtChangeColor :: RGBARange -> RGBA -> QuadTree RGBA -> QuadTree RGBA
qtChangeColor range target = fmap $ pixelChangeColor range target

qtSaturate :: Double -> QuadTree RGBA -> QuadTree RGBA
qtSaturate c = fmap $ pixelSaturate c

qtGrayscale :: QuadTree RGBA -> QuadTree RGBA
qtGrayscale = fmap pixelGrayScale

qtBlur :: QuadTree e -> Int -> QuadTree e
qtBlur = undefined

-- ints: upper left corner (x, y) and size (w, l)
qtCrop :: Int -> Int -> Int -> Int -> QuadTree e -> QuadTree e
qtCrop = undefined

-- Fails on Rotate Left
-- QT (Leaf ((202.31416365064572,12.587193235712554,96.80080352156087,160.01543169945634),1,1))
--   (LeafList (PL {isHorizontal = True, pixelData = [(208.5484188328414,13.366259421433847,10.294411286895267,131.0292175135899),(252.73349511527314,7.5814132834679295,197.15628850928678,242.7895447414606)]}))
--   (Leaf ((160.9124223339754,150.13605287848264,24.02981108240578,109.04092531929763),1,1))
--   (LeafList (PL {isHorizontal = True, pixelData = [(122.12805696893412,68.76566534170779,124.11001691173823,209.47069433619214),(53.65679534898248,147.87785146878716,229.79229822700026,236.41426661530258)]})) 2 3
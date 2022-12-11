module QuadTree (
    QuadTree (..),
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
import PPM qualified as P (PPM, RGBA)
import Pixel

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
sameQTColor _ _ = False

color :: QuadTree e -> Maybe e
color (Leaf (x, _, _)) = Just x
color _ = Nothing

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

compress :: P.PPM -> QuadTree P.RGBA
compress = buildQuadTree

decompress :: QuadTree P.RGBA -> P.PPM
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
qtChangeColor :: RGBARange -> P.RGBA -> QuadTree RGBA -> QuadTree RGBA
qtChangeColor range target = fmap $ pixelChangeColor range target

qtSaturate :: Double -> QuadTree RGBA -> QuadTree RGBA
qtSaturate c = fmap $ pixelSaturate c

qtGrayscale :: QuadTree RGBA -> QuadTree RGBA
qtGrayscale = fmap pixelGrayScale

qtBlur :: QuadTree e -> Int -> QuadTree e
qtBlur = undefined

-- ints: upper left corner (x, y) and size (w, l)
qtCrop :: QuadTree e -> Int -> Int -> Int -> Int -> QuadTree e
qtCrop = undefined
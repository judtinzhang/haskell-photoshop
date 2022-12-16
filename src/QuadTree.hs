module QuadTree (
    QuadTree (..),
    PixelList (..),
    compress,
    decompress,
    readInput,
    toJpg,
    toPng,
    rotateLeft,
    rotateRight,
    reflectHorizontal,
    reflectVertical,
    changeColor,
    saturate,
    grayscale,
    blur,
    getColor,
    crop,
    recompress,
    lossyCompress
) where

import Codec.Picture
import Data.Foldable qualified as Foldable
import PPM qualified as P (PPM, crop, readInput, neighborList)
import Pixel
import Data.Maybe (isJust)

-- ================= IO ===========================

readInput :: String -> IO (QuadTree RGBA)
readInput s = do
  input <- P.readInput s
  return $ compress input

convertToImage :: QuadTree RGBA -> Image PixelRGBA8
convertToImage i =
  generateImage gen w h
  where
    (h, w) = size i
    gen x y =
      case getColor y x i of
        Nothing -> error "Error: Dimensions out of bounds"
        Just (r, g, b, a) ->
          PixelRGBA8 (toPixel8 r) (toPixel8 g) (toPixel8 b) (toPixel8 a)

toJpg :: QuadTree RGBA -> String -> IO ()
toJpg p out_file =
  let image = convertToImage p
   in saveJpgImage 100 out_file (ImageRGBA8 image)

toPng :: QuadTree RGBA -> String -> IO ()
toPng p out_file =
  let image = convertToImage p
   in savePngImage out_file (ImageRGBA8 image)

-- Represents a 1 x n or n x 1 set of pixels in our QuadTree,
-- which cannot be further compressed
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

-- ================= IO for QuadTree ===========================

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

recompress :: Eq e => QuadTree e -> QuadTree e
recompress qt@(QT tl tr bl br h w) =
  if sameQTColor tl tr && sameQTColor tr bl && sameQTColor bl br
    then case color tl of
      Just c -> Leaf (c, h, w)
      Nothing -> error "Error: Impossible color"
    else QT (recompress tl) (recompress tr) (recompress bl) (recompress br) h w
recompress x = x

-- Compresses the QuadTree in a lossy fashion, so as to not
-- preserve all pixels and instead combine pixels that have similar
-- RGBA values
lossyCompress :: Int -> QuadTree RGBA -> QuadTree RGBA
lossyCompress s (QT tl tr bl br h w) =
  let tl_loss = lossyCompress s tl in
  let tr_loss = lossyCompress s tr in
  let bl_loss = lossyCompress s bl in
  let br_loss = lossyCompress s br in
  if closeQTColor s tl_loss tr_loss
    && closeQTColor s tr_loss bl_loss
    && closeQTColor s bl_loss br_loss
    then case lossyColor s tl_loss of
      Just c -> Leaf (c, h, w)
      Nothing -> error "impossible color"
    else QT tl_loss tr_loss bl_loss br_loss h w
lossyCompress _ x = x

-- ================= QuadTree Internal Functions ===========================

-- x1, y2 are inclusive, x2, y2 (exclusive)
-- NOTE: this is different from PPM's getSubArray function
getSubArray :: Int -> Int -> [a] -> [a]
getSubArray start end array = take (end - start) (drop start array)

-- Gets submatrix between startRow, endRow, startCol, endCol
getSubMatrix :: Int -> Int -> Int -> Int -> [[a]] -> [[a]]
getSubMatrix x1 x2 y1 y2 ppm = map (getSubArray x1 x2) (getSubArray y1 y2 ppm)

-- Returns size of QuadTree in form (h, w)
size :: QuadTree e -> (Int, Int)
size (QT _ _ _ _ h w) = (h, w)
size (LeafList pl) =
  if isHorizontal pl
    then (1, length $ pixelData pl)
    else (length $ pixelData pl, 1)
size (Leaf (_, h, w)) = (h, w)

-- Checks if two QuadTrees have same color (note this only applies
-- to leaves and LeafLists)
sameQTColor :: Eq e => QuadTree e -> QuadTree e -> Bool
sameQTColor (Leaf (x, _, _)) (Leaf (y, _, _)) = x == y
sameQTColor (Leaf (x, _, _)) (LeafList pl) =
  let (b, c) = colorPL pl in
  b && (c == x)
sameQTColor (LeafList pl) (Leaf (y, _, _)) =
  let (b, c) = colorPL pl in
  b && (c == y)
sameQTColor (LeafList pl1) (LeafList pl2) =
  let (b1, c1) = colorPL pl1 in
  let (b2, c2) = colorPL pl2 in
  b1 && b2 && (c1 == c2)
sameQTColor _ _ = False

-- Checks if two QuadTrees have similar color range (note this only applies
-- to leaves and LeafLists)
closeQTColor :: Int -> QuadTree RGBA -> QuadTree RGBA -> Bool
closeQTColor s (Leaf (x, _, _)) (Leaf (y, _, _)) = closeColor s x y
closeQTColor s (Leaf (x, _, _)) (LeafList pl) =
  let (b, c) = closeColorPL s pl in
  b && closeColor s x c
closeQTColor s (LeafList pl) (Leaf (y, _, _)) =
  let (b, c) = closeColorPL s pl in
  b && closeColor s y c
closeQTColor s (LeafList pl1) (LeafList pl2) =
  let (b1, c1) = closeColorPL s pl1 in
  let (b2, c2) = closeColorPL s pl2 in
  b1 && b2 && closeColor s c1 c2
closeQTColor _ _ _ = False

-- Returns the color of a QuadTree (note this only applies to
-- leaves and LeafLists)
color :: Eq e => QuadTree e -> Maybe e
color (Leaf (x, _, _)) = Just x
color (LeafList pl) =
  let (b, c) = colorPL pl in
    if b then Just c else Nothing
color _ = Nothing

-- Returns whether two colors are similar with some magnitude
closeColor :: Int -> RGBA -> RGBA -> Bool
closeColor s (r1, g1, b1, a1) (r2, g2, b2, a2) =
  round (abs (r1 - r2)) < s &&
  round (abs (g1 - g2)) < s &&
  round (abs (b1 - b2)) < s &&
  round (abs (a1 - a2)) < s

-- Returns the color of the lossy RGBA value
lossyColor :: Int -> QuadTree RGBA -> Maybe RGBA
lossyColor s (Leaf (x, _, _)) = Just x
lossyColor s (LeafList pl) =
  let (b, c) = closeColorPL s pl in
    if b then Just c else Nothing
lossyColor _ _ = Nothing

-- Returns the color of a PixelList
colorPL :: Eq e => PixelList e -> (Bool, e)
colorPL pl = foldr (\x (b, c) -> ((x == c) && b, c)) (True, head $ pixelData pl) pl

-- Returns the color of the closest RGBA value of a PixelList
closeColorPL :: Int -> PixelList RGBA -> (Bool, RGBA)
closeColorPL s pl = foldr
  (\x (b, c) -> (closeColor s x c && b, c)) (True, head $ pixelData pl) pl

-- Gets the color of a given x, y coordinate in a QuadTree
getColor :: Int -> Int -> QuadTree e -> Maybe e
getColor y x (Leaf (c, h, w)) =
  if y < 0 || x < 0 || y >= h || x >= w
    then Nothing
    else Just c
getColor y x (LeafList pl) =
  let len = length $ pixelData pl in
  if isHorizontal pl && (y /= 0 || x < 0 || x >= len)
    || not (isHorizontal pl) && (x /= 0 || y < 0 || y >= len)
    then Nothing
    else Just $ if isHorizontal pl then pixelData pl !! x else pixelData pl !! y
getColor y x (QT tl tr bl br h w) =
  if y < 0 || x < 0 || y >= h || x >= w
    then Nothing
    else
      let y_mid = fst $ size tl in
      let x_mid = snd $ size tl in
      if y < y_mid then
        if x < x_mid
          then getColor y x tl
          else getColor y (x - x_mid) tr
      else
        if x < x_mid
          then getColor (y - y_mid) x bl
          else getColor (y - y_mid) (x - x_mid) br

-- Returns whether a given x y coordinate is valid inside the QuadTree
validIndex :: QuadTree RGBA -> Int -> Int -> Bool
validIndex qt i j = isJust $ getColor i j qt

-- ================= Image Processing Functions ===========================

rotateLeft :: QuadTree e -> QuadTree e
rotateLeft (Leaf (x, i, j)) = Leaf (x, j, i)
rotateLeft (LeafList pl) =
    LeafList PL {
      isHorizontal = not $ isHorizontal pl,
      pixelData = if isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
rotateLeft (QT tl tr bl br h w) = QT
  (rotateLeft tr)
  (rotateLeft br)
  (rotateLeft tl)
  (rotateLeft bl)
  w
  h

rotateRight :: QuadTree e -> QuadTree e
rotateRight (Leaf (x, i, j)) = Leaf (x, j, i)
rotateRight (LeafList pl) =
    LeafList PL {
      isHorizontal = not $ isHorizontal pl,
      pixelData = if not $ isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
rotateRight (QT tl tr bl br h w) = QT
  (rotateRight bl)
  (rotateRight tl)
  (rotateRight br)
  (rotateRight tr)
  w
  h

reflectHorizontal :: QuadTree e -> QuadTree e
reflectHorizontal (Leaf x) = Leaf x
reflectHorizontal (LeafList pl) =
    LeafList PL {
      isHorizontal = isHorizontal pl,
      pixelData = if isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
reflectHorizontal (QT tl tr bl br h w) = QT
  (reflectHorizontal tr)
  (reflectHorizontal tl)
  (reflectHorizontal br)
  (reflectHorizontal bl)
  h
  w

reflectVertical :: QuadTree e -> QuadTree e
reflectVertical (Leaf x) = Leaf x
reflectVertical (LeafList pl) =
    LeafList PL {
      isHorizontal = isHorizontal pl,
      pixelData = if not $ isHorizontal pl
        then reverse $ pixelData pl
        else pixelData pl
    }
reflectVertical (QT tl tr bl br h w) = QT
  (reflectVertical bl)
  (reflectVertical br)
  (reflectVertical tl)
  (reflectVertical tr)
  h
  w

changeColor :: RGBARange -> RGBA -> QuadTree RGBA -> QuadTree RGBA
changeColor range target qt = recompress ((fmap $ pixelChangeColor range target) qt)

saturate :: Double -> QuadTree RGBA -> QuadTree RGBA
saturate c qt = recompress ((fmap $ pixelSaturate c) qt)

grayscale :: QuadTree RGBA -> QuadTree RGBA
grayscale qt = recompress (fmap pixelGrayScale qt)

-- Note: crop only makes sense in the context of PPM, so
-- QuadTree crop uses the PPM crop
crop :: Int -> Int -> Int -> Int -> QuadTree RGBA -> QuadTree RGBA
crop r1 r2 c1 c2 qt = compress (P.crop r1 r2 c1 c2 (decompress qt))

blur :: QuadTree RGBA -> Int -> QuadTree RGBA
blur qt@(Leaf _) _ = qt
blur qt@(LeafList pl) radius =
  let h = if isHorizontal pl then 1 else length $ pixelData pl in
  let w = if isHorizontal pl then length $ pixelData pl else 1 in
  compress (blurHelper qt 0 radius h w)
blur qt@(QT tl tr bl br h w) radius = compress (blurHelper qt 0 radius h w)

-- ================= Helper functions for User Functions ===========================

blurHelper :: QuadTree RGBA -> Int -> Int -> Int -> Int -> P.PPM
blurHelper qt v radius h w =
  if v < h
    then blurRow qt v 0 radius w : blurHelper qt (v + 1) radius h w
    else []

blurRow :: QuadTree RGBA -> Int -> Int -> Int -> Int -> [RGBA]
blurRow qt i j radius w =
  if j < w
    then neighborAvg i j radius qt : blurRow qt i (j + 1) radius w
    else []

-- Updates running sum and count of all neighbor RGBA values
updateAvg :: QuadTree RGBA -> Int -> Int -> (RGBA, Double) -> (RGBA, Double)
updateAvg qt i j t@((r, g, b, a), count) =
  if validIndex qt i j
    then
      case getColor i j qt of
        Nothing -> error "Get Color should never return Nothing here"
        Just c ->
          let (r', g', b', a') = c
          in ((r + r', g + g', b + b', a + a'), count + 1)
    else t

-- Averages all neighbor RGBA values given a radius
neighborAvg :: Int -> Int -> Int -> QuadTree RGBA -> RGBA
neighborAvg i j radius qt =
  let nl = P.neighborList i j radius
   in let (p, count) = foldr (\(i, j) acc -> updateAvg qt i j acc) ((0, 0, 0, 0), 0) nl
       in mapRGBA (/ count) p

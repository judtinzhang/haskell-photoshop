module QuadTree (
    QuadTree (..),
    compress,
    decompress,
    qtRotate,
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

-- import PPM qualified as P (RGBA)


data PixelList e = PL {
  isHorizontal :: Bool,
  pixelData :: [e]
} deriving (Show, Eq)

instance Foldable PixelList where
  foldMap f pl = foldMap f (pixelData pl)

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
  deriving (Show, Foldable, Eq)

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


mat :: [[Int]]
mat = [[1, 1, 1, 4], [1, 3, 3, 4], [1, 3, 3, 5]]

qt :: QuadTree Int
qt = buildQuadTree mat

testQT :: IO ()
testQT = do
  -- print qt
  let ppm = buildPPM qt
  print ppm

compress :: P.PPM -> QuadTree P.RGBA
compress = buildQuadTree

decompress :: QuadTree P.RGBA -> P.PPM
decompress = buildPPM


-- -- | List the elements in the tree, in ascending order
-- elements :: QuadTree e -> [e]
-- elements = Foldable.toList






-- import Data.Maybe (fromMaybe)

-- -- A point in 2D space
-- type Point = (Double, Double)

-- -- A rectangle in 2D space
-- type Rect = (Point, Point)

-- -- A quadtree node
-- data QuadTree a = Leaf a | Node (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)

-- -- Builds a quadtree from a list of points
-- buildQuadTree :: [Point] -> QuadTree [Point]
-- buildQuadTree points =
--     let (minX, minY, maxX, maxY) = foldl (\(minX, minY, maxX, maxY) (x, y) -> (min x minX, min y minY, max x maxX, max y maxY)) (1/0, 1/0, -1/0, -1/0) points
--         boundingBox = ((minX, minY), (maxX, maxY))
--         splitBox (p1, p2) = let (x1, y1) = p1
--                                 (x2, y2) = p2
--                                 xm = (x1 + x2) / 2
--                                 ym = (y1 + y2) / 2
--                             in [(p1, (xm, ym)), ((xm, y1), (x2, ym)), ((x1, ym), (xm, y2)), ((xm, ym), p2)]
--         splitPoints [] _ = []
--         splitPoints (p:ps) [] = splitPoints ps boundingBox
--         splitPoints points (b:bs) = let (inPoints, outPoints) = partition (\p -> inside p b) points
--                                     in inPoints : splitPoints outPoints bs
--         inside (x, y) (((x1, y1), (x2, y2)),) = x1 <= x && x <= x2 && y1 <= y && y <= y2
--     in buildQuadTree' (splitPoints points (splitBox boundingBox))

-- -- Builds a quadtree from a list of point lists
-- buildQuadTree' :: [[Point]] -> QuadTree [Point]
-- buildQuadTree' [] = Leaf []
-- buildQuadTree' [points] = Leaf points
-- buildQuadTree' (a:b:c:d:[]) = Node (buildQuadTree' [a]) (buildQuadTree' [b]) (buildQuadTree' [c]) (buildQuadTree' [d])

-- -- Finds the points in a quadtree that are inside a given rectangle
-- findPoints :: QuadTree [Point] -> Rect -> [Point]
-- findPoints (Leaf points) rect = filter (\p -> inside p rect) points
--     where inside (x, y) (((x1, y1), (x2, y2)),) = x1 <= x && x <= x2 && y1 <= y && y <= y2
-- findPoints (Node a b c d) rect = concatMap (findPoints rect) [a, b, c, d]

-- -- Finds the bounding box of a quadtree
-- boundingBox :: QuadTree a -> Rect
-- boundingBox (Leaf points) = ((minimum (map fst points), minimum (map




-- *******************************************


qtRotate :: QuadTree e -> Int -> QuadTree e
qtRotate = undefined

qtReflectHorizontal :: QuadTree e -> QuadTree e
qtReflectHorizontal = undefined

qtReflectVertical :: QuadTree e -> QuadTree e
qtReflectVertical = undefined

qtChangeColor :: QuadTree e -> P.RGBA -> QuadTree e
qtChangeColor = undefined

qtSaturate :: QuadTree e -> Int -> QuadTree e
qtSaturate = undefined

qtGrayscale :: QuadTree e -> Int -> QuadTree e
qtGrayscale = undefined

qtBlur :: QuadTree e -> Int -> QuadTree e
qtBlur = undefined

-- ints: upper left corner (x, y) and size (w, l)
qtCrop :: QuadTree e -> Int -> Int -> Int -> Int -> QuadTree e
qtCrop = undefined
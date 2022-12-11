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
    qtCrop
) where

import Data.Foldable qualified as Foldable
import PPM qualified as P (PPM, RGBA)

-- import PPM qualified as P (RGBA)

compress :: P.PPM -> QuadTree e
compress = undefined

decompress :: QuadTree e -> P.PPM
decompress = undefined

data QuadTree e
  = Leaf e -- empty QuadTree
  | QT -- non-empty tree, with...
      (QuadTree e)
      (QuadTree e)
      (QuadTree e)
      (QuadTree e)
      Int -- height
  deriving (Show, Foldable, Eq)

-- | List the elements in the tree, in ascending order
elements :: QuadTree e -> [e]
elements = Foldable.toList

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
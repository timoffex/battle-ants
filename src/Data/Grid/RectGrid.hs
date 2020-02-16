module Data.Grid.RectGrid
    ( RectGrid (..)
    , gridMinXY
    , gridMaxXY
    , gridMinRC
    , gridMaxRC
    , nrows
    , ncols
    , minXY
    , maxXY
    , minRC
    , maxRC
    , rectBetween
    ) where

import Data.Grid.Grid
import Data.Grid.GridPosition

minXY :: GridPosition -> GridPosition -> (Int, Int)
minXY p1 p2 = let (p1x, p1y) = toXY p1
                  (p2x, p2y) = toXY p2
              in (min p1x p2x, min p1y p2y)

maxXY :: GridPosition -> GridPosition -> (Int, Int)
maxXY p1 p2 = let (p1x, p1y) = toXY p1
                  (p2x, p2y) = toXY p2
              in (max p1x p2x, max p1y p2y)

minRC :: GridPosition -> GridPosition -> (Int, Int)
minRC p1 p2 = let (p1r, p1c) = toRowColumn p1
                  (p2r, p2c) = toRowColumn p2
              in (min p1r p2r, min p1c p2c)

maxRC :: GridPosition -> GridPosition -> (Int, Int)
maxRC p1 p2 = let (p1r, p1c) = toRowColumn p1
                  (p2r, p2c) = toRowColumn p2
              in (max p1r p2r, max p1c p2c)


-- | Enumerates the rectangle of positions between the two given ones.
rectBetween :: GridPosition -> GridPosition -> [GridPosition]
rectBetween p1 p2 = let (minX, minY) = minXY p1 p2
                        (maxX, maxY) = maxXY p1 p2
                    in [ fromXY (x, y)
                       | x <- [minX .. maxX]
                       , y <- [minY .. maxY] ]

-- | A grid bounded by a rectangle.
class (RigidGrid g) => RectGrid g where
  corners :: g a -> (GridPosition, GridPosition)

gridMinXY :: RectGrid g => g a -> (Int, Int)
gridMinXY = uncurry minXY . corners

gridMaxXY :: RectGrid g => g a -> (Int, Int)
gridMaxXY = uncurry maxXY . corners

gridMinRC :: RectGrid g => g a -> (Int, Int)
gridMinRC = uncurry minRC . corners

gridMaxRC :: RectGrid g => g a -> (Int, Int)
gridMaxRC = uncurry maxRC . corners

nrows :: (RectGrid g) => g a -> Int
nrows g = let (r1, _) = toRowColumn $ fst $ corners g
              (r2, _) = toRowColumn $ snd $ corners g
          in 1 + abs (r1 - r2)

ncols :: (RectGrid g) => g a -> Int
ncols g = let (_, c1) = toRowColumn $ fst $ corners g
              (_, c2) = toRowColumn $ snd $ corners g
          in 1 + abs (c1 - c2)

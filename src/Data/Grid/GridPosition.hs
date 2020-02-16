{-# LANGUAGE NoImplicitPrelude #-}

module Data.Grid.GridPosition
    ( GridPosition
    , toRowColumn
    , fromRowColumn
    , toXY
    , fromXY
    ) where

import NumHask.Prelude hiding (Grid)
import NumHask.Algebra.Abstract.Additive

newtype GridPosition = GridPosition (Int, Int)
                       deriving (Eq, Show, Ord)

toRowColumn :: GridPosition -> (Int, Int)
toRowColumn (GridPosition (x, y)) = (negate y, x)

fromRowColumn :: (Int, Int) -> GridPosition
fromRowColumn (r, c) = fromXY (c, negate r)

toXY :: GridPosition -> (Int, Int)
toXY (GridPosition p) = p

fromXY :: (Int, Int) -> GridPosition
fromXY = GridPosition

instance Additive GridPosition where
  (GridPosition (x, y)) + (GridPosition (x', y')) = GridPosition (x+x',y+y')
  zero = fromXY (0, 0)

instance Subtractive GridPosition where
  negate (GridPosition (x, y)) = GridPosition (-x, -y)
